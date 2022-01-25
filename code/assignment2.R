# install.packages("gender")
# install.packages("wru")
# install.packages("lubridate")
# install.packages('arrow')

library('arrow')
library('gender')
library('wru')
library('dplyr')

# Import data:
app_data = read_parquet('code/app_data_sample.parquet')
attach(app_data)

# gender function
# gender("madison")

# wru
app_data['surname'] = app_data['examiner_name_last']
attach(app_data)

#predictions = predict_race(voter.file = app_data, surname.only = T)
attach(predictions)

final_data <- transform(predictions, pred.race = pmax(pred.whi, pred.bla, 
                                                      pred.his, pred.asi, 
                                                      pred.oth))
attach(final_data)

final_data <- transform(final_data, race = ifelse(pred.race == pred.whi, "white", 
                                                  ifelse(pred.race == pred.bla, "black", 
                                                         ifelse(pred.race == pred.his, "hispanic", 
                                                                ifelse(pred.race == pred.asi, "asian", "other")))))

examiners_byartunit <- final_data %>% group_by(examiner_art_unit, race)
count_byraceinartunit <- examiners_byartunit %>% tally()




