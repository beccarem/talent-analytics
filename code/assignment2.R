# install.packages("gender")
# install.packages("wru")
# install.packages("lubridate")
# install.packages('arrow')

library('arrow')
library('gender')
library('wru')
library('dplyr')
library('zoo')

# Import data:
app_data = read_parquet('code/app_data_sample.parquet')
attach(app_data)

# wru - change the last name column to surname
app_data['surname'] = app_data['examiner_name_last']
attach(app_data)

# prediction of race percentages - takes a long time to run
predictions = predict_race(voter.file = app_data, surname.only = T)
attach(predictions)

# finding the highest probability
predictions_final <- transform(predictions, pred.race = pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth))
attach(predictions_final)

# added "race" column
predictions_final <- transform(predictions_final, race = ifelse(pred.race == pred.whi, "white", 
                                                                ifelse(pred.race == pred.bla, "black", 
                                                                      ifelse(pred.race == pred.his, "hispanic", 
                                                                            ifelse(pred.race == pred.asi, "asian", "other")))))

# added count of examiner by race in art unit
examiners_byrace_inartunit <- predictions_final %>% group_by(examiner_art_unit, race)
count_byraceinartunit <- examiners_byrace_inartunit %>% tally()

# quarters
# quart <- as.yearqtr(app_data$filing_date, format = "%Y-%m-%d")
# quart


