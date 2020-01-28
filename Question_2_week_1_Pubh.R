#load necessary packages
library(tidyverse)
library(dplyr)
library(purrr)
library(tibble)
library(plotly)
library(magrittr)
library(data.table)
library(fastDummies)
library(corrplot)


#load in data for framingham.csv into a dataframe called framingham_v1 - gender is being brough in as a factor 
framingham <- read.csv("framingham.csv",header = TRUE)


#convert framingham to data.table to speed up things, not useful in this case but why not
framingham <- data.table(framingham_v2)


#Creating framingham_full for men, and women where BP is higher than 0 to filter out any missing/negative results 
framingham_full <- framingham %>% filter(gender == "Female" | gender == "Male" & sysBP > 0) %>% mutate(ShockIndex = heartRate/sysBP) 

#rounding shock index to two sigfigs
#framingham_full$ShockIndex <- round(framingham_full$ShockIndex,2)

#changing diabetic to factor 
framingham_full$diabetes <- as.factor(framingham_full$diabetes)

#creating levels, not really useful but gfood to have for further analysis
levels(framingham_full$diabetes) <- c("Not Diabetic","Diabetic")

#creating dummy columns
framingham_full <- framingham_full %>% dummy_cols()


#plotting the results - whisker plot
plot_ly(framingham_full, y = ~ShockIndex, color = ~diabetes, type = "box", jitter = 0.5)

paste("here are some summary statistics by groups for both genders")
stats_by_diabetic_status <- framingham_full %>% group_by(diabetes) %>% summarise(counts = n(), mean = mean(ShockIndex,na.rm = T), median(ShockIndex,na.rm = T), sd(ShockIndex, na.rm = T), min(ShockIndex, na.rm = T),max(ShockIndex, na.rm = T))
stats_by_diabetic_status
#paste("summry statistics using base r")
#do.call("rbind",tapply(framingham_full$ShockIndex,framingham_full$diabetes,quantile, na.rm = T))

#calculating proportion of population to see what percentage is above .9 shock index
#framingham_full %>% filter(ShockIndex > .9) %>% count(diabetes)

#dropped several columns here 
#corrplot(cor(framingham_full_char[1:18]))