rm(list = ls())

setwd("C:/Users/riosn/OneDrive/Documents/GMU/Papers in Progress/ConstrainedOOFA")

clean_survey_data = read.csv("clean_survey_data_4162024.csv")

# filter out times that took more than 2 hours (this doesn't seem to change the score_model much)
outlier_ids = which(clean_survey_data$Duration..in.seconds. > 7200)
if(length(outlier_ids) == 0){
  clean_data = clean_survey_data
} else{
  clean_data = clean_survey_data[-outlier_ids,]  
}

clean_data$duration = clean_data$Duration..in.seconds.

score_model = lm(Score ~ z12 + z34 + z35 + z36 + z46 + z56, data = clean_data)
summary(score_model)


library(dplyr)
library(car)

# set.seed(123) # random sample 1
set.seed(1234) # random sample 2

uniform_data = clean_data %>% group_by(Order) %>% sample_n(size = 1)

score_model2 = lm(Score ~ z12 + z34 + z35 + z36 + z46 + z56, data = uniform_data)
summary(score_model2)
shapiro.test(score_model2$residuals)
ncvTest(score_model2, ~ z12 + z34 + z35 + z36 + z46 + z56)



