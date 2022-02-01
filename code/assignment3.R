################################## SETUP #######################################

##### setup -----
rm(list = ls())
gc()

library(tidyverse)
library(skimr)
library(lubridate)
library(arrow)
library(modelsummary)
set.seed(10101)

##### paths -----
data_path = "USPTO_data/"

###### S4 regressions -----
###### > some prep -----
# Load examiner GS data
examiner_gs <- read_csv(paste0(data_path,"examiner_gs.csv"))

##### >> calculate duration in grade -----
# Use latest observed date to replace NAs
max_end_date <- examiner_gs %>% 
  summarise(max(mdy(end_date,"m/d/y"), na.rm = TRUE)) %>% 
  pull()
# get number of days in a GS grade
examiner_gs <- examiner_gs %>% 
  mutate(
    start_date_old = start_date, # for manual verification
    end_date_old = end_date, # for manual verification
    start_date = mdy(start_date_old),
    end_date = if_else(is.na(end_date),max_end_date,mdy(end_date)),
    days_in_grade = interval(start_date, end_date) %/% days(1)
  ) %>% 
  select(-start_date_old,-end_date_old)


##### >> add examiner gender -----
# Using a modified example from https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html
library(gender)
#install_genderdata_package() # only run the first time
examiner_gender <- examiner_gs %>% 
  mutate(
    name = examiner_name,
    ff_name = str_extract(examiner_name,"(?<=,\\s)\\w+"), # extract first first name
  ) %>% 
  distinct(ff_name) %>% 
  do(results = gender(.$ff_name, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    ff_name = name,
    gender,
    proportion_female
  )
# joining gender back to the dataset
examiner_gs <- examiner_gs %>% 
  mutate(
    ff_name = str_extract(examiner_name,"(?<=,\\s)\\w+"), # extract first first name
  ) %>% 
  left_join(examiner_gender, by = "ff_name")
# cleaning up
rm(examiner_gender)
gc()

# load AU data
examiner_au <- read_csv(paste0(data_path,"examiner_aus.csv"))

# count examiner moves
examiner_moves <- examiner_au %>% 
  arrange(old_pid,year,month) %>% 
  distinct(old_pid,examiner_art_unit) %>% # keep unique examiner-AU combinations
  group_by(old_pid) %>% 
  mutate(
    au = examiner_art_unit,
    tc = floor(au/100)*100,
    moves = (n()-1)
  ) %>% 
  ungroup()

datasummary_skim(examiner_moves, histogram=FALSE)

# add examiner gender and tenure
examiner_gender_tenure <- examiner_gs %>% 
  filter(!is.na(gender)) %>% 
  mutate(
    woman = if_else(gender=="female",1,0)
  ) %>%
  group_by(old_pid) %>% 
  summarise(
    woman = max(woman),
    tenure = sum(days_in_grade)/365
  )

datasummary_skim(examiner_gender_tenure, histogram=FALSE)

# remove negative tenure values
examiner_gender_tenure <- examiner_gender_tenure %>% 
  filter(tenure>0)

# add back to the examiner_moves
examiner_moves <- examiner_moves %>% 
  left_join(examiner_gender_tenure)

datasummary_skim(examiner_gender_tenure, histogram=FALSE)

# clean up
rm(examiner_gender_tenure)
gc()

# load model packages
library(modelr)
library(infer)

################################ PREDICTION ####################################

# Target: Moves from examiner_moves table
# Predictors: examiner_grade, days_in_grade, woman, proportion_female,
#             tc, tenure

library(randomForest)
library(rpart)
require(caTools)
library(ggplot2)

# merge tables
finaldf <- merge(examiner_gs, examiner_moves, by="old_pid")
finaldf <- drop_na(finaldf)
finaldf <- transform(finaldf, moveindicator = ifelse(moves==0, 0, 1))

# subset of finaldf
subsetdf <- finaldf[1:10000,]

# train and test sample split
sample = sample.split(finaldf, SplitRatio = .70)
train = subset(finaldf, sample == TRUE)
test  = subset(finaldf, sample == FALSE)

# models
# rf = randomForest(moves ~ examiner_grade + days_in_grade + woman +
#                   proportion_female + tc + tenure, data=train, trees=100)
# rf

dt = rpart(moveindicator ~ examiner_grade + days_in_grade + woman + 
             proportion_female + tc + tenure, data=train)
dt

# prediction with test data
pred <- c(predict(dt, newdata=test))
test['movepred'] <- pred

# ROC stuff
# true positive rate
func_tpr <- function(pred) {
  vec = 0
  for (i in 1:length(pred)) {
    if (pred[i] == 1 & test$moveindicator[i] == 1) {
      vec = vec + 1
    }
  }
  vec = vec / length(pred)
  return(vec)
}

# false positive rate
func_fpr <- function(pred) {
  vec = 0
  for (i in 1:length(pred)) {
    if (pred[i] == 1 & test$moveindicator[i] == 0) {
      vec = vec + 1
    }
  }
  vec = vec / length(pred)
  return(vec)
}

# # true negative rate
# func_tnr <- function(pred) {
#   vec = 0
#   for (i in 1:length(pred)) {
#     if (pred[i] == 0 & test$moveindicator[i] == 0) {
#       vec = vec + 1
#     }
#   }
#   vec = vec / length(pred)
#   return(vec)
# }
# 
# # false negative rate
# func_fnr <- function(pred) {
#   vec = 0
#   for (i in 1:length(pred)) {
#     if (pred[i] == 0 & test$moveindicator[i] == 1) {
#       vec = vec + 1
#     }
#   }
#   vec = vec / length(pred)
#   return(vec)
# }

# cutoff stuff
tpr <- c(0)
fpr <- c(0)

# 0.6
pred06 = ifelse(test['movepred'] < 0.6, 0, 1)
tpr <- append(tpr, func_tpr(pred06))
fpr <- append(fpr, func_fpr(pred06))

# 0.7
pred07 = ifelse(test['movepred'] < 0.7, 0, 1)
tpr <- append(tpr, func_tpr(pred07))
fpr <- append(fpr, func_fpr(pred07))

# 0.8
pred08 = ifelse(test['movepred'] < 0.8, 0, 1)
tpr <- append(tpr, func_tpr(pred08))
fpr <- append(fpr, func_fpr(pred08))

# 0.9
pred09 = ifelse(test['movepred'] < 0.9, 0, 1)
tpr <- append(tpr, func_tpr(pred09))
fpr <- append(fpr, func_fpr(pred09))

# add points (1, 1)
tpr <- append(tpr, 1)
fpr <- append(fpr, 1)

df <- data.frame(tpr, fpr)
# tpr: 0.0000000 0.8899976 0.8665852 0.6661802 0.6661802 1.0000000
# fpr: 0.00000000 0.11000238 0.09823663 0.03386751 0.03386751 1.00000000

# ROC plots
g <- ggplot(df, aes(x = fpr, y = tpr))

g + geom_point() + 
    geom_line(color="blue") +
    geom_abline(intercept = 0) +
    xlim(0,1) +
    ylim(0,1)

# AUC
# load the rgeos library
library(rgeos)

# make a polygon (borrowed from ref manual for package)
sample_polygon <- readWKT("POLYGON((0 0, 0.11 0.89, 0.1 0.9, 0.03 0.67, 1 1, 0 0))")

# and calculate the area
gArea(sample_polygon)
# AUC: 0.295


