################################## TM2: TCs ####################################
library(tidyverse)

# import examiner_aus.csv
data_path = "USPTO_data/"
examiner_aus <- read.csv(paste0(data_path,"examiner_aus.csv"))

# add new column tc
examiner_aus["tc"] <- floor(examiner_aus$examiner_art_unit/100)*100

# table(examiner_au$years)
# 2003  2004  2005  2006  2007  2008  2009  2010  2011  2012 
# 4609 33465 47644 51129 47774 66289 76085 75029 62968 76832 

# length(unique(examiner_au$tc))
# 97

# count how many in each in 2000
count_tc <- examiner_aus %>% group_by(year, tc, examiner_art_unit) %>% summarise(n = n())
write.csv(count_tc, "tc.csv")
