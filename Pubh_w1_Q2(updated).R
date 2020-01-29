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
framingham <- read.csv("framingham_umn.csv",header = TRUE)


#convert framingham to data.table to speed up things, not useful in this case but why not
framingham <- data.table(framingham)


#Creating framingham_full for men, and women where BP is higher than 0 to filter out any missing/negative results 
framingham_full <- framingham %>% filter(gender == "Female" | gender == "Male" & sysBP > 0) %>% mutate(ShockIndex = heartRate/sysBP) 


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

#calculating proportion of population to see what percentage is above .9 shock index
f1 <- framingham_full %>% group_by(diabetes) %>% filter(ShockIndex > .9) %>% count(diabetes)
f2 <- framingham_full %>% group_by(diabetes) %>% count(diabetes)
percentage_by_population <- (f1$n/f2$n) * 100
population_proportion <- cbind(as.data.frame(f1),as.data.frame(percentage_by_population))

#renaming columns
colnames(population_proportion)[1] <- "Is patient diabetic?"
colnames(population_proportion)[2] <- "# of people with SI > .9"

#printing population_proportion
population_proportion <- as.data.frame(population_proportion)
population_proportion

if (population_proportion[2,3] > population_proportion[1,3]){
  print ("ShockIndex proportion is higher in diabetic population")
} else {
  print ("ShockIndex proportion is low in non-diabetic population")
}




