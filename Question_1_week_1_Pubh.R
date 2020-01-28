#load necessary packages
library(tidyverse)
library(dplyr)
library(purrr)
library(tibble)
library(plotly)
library(magrittr)
library(data.table)
library(fastDummies)

#load in data for framingham.csv into a dataframe called framingham_v1 - gender is being brough in as a factor 
framingham <- read.csv("framingham.csv",header = TRUE)




#convert framingham_v1 to data.table to speed up things, not useful in this case but why not
framingham <- data.table(framingham)


#filter for only women & whose sysBP was measured and only keeping the entries that are greater than 0. The new result
#is also saved as framingham_women
framingham_women <- framingham %>% filter(gender == "Female" & sysBP > 0 )



#count total number of missing values in the data frame - ignoring this for now
#paste("Total number of NA entries is",sum(is.na(framingham_women)),"in your dataframe")


#Adding ShockIndex column CholRisk column and rounding ShockIndex to 2 sigfigs
framingham_women <- framingham_women %>% mutate(ShockIndex = heartRate/sysBP) %>% mutate(cholRisk = cut(framingham_women$totChol, breaks = c(0,200,240,600))) 
framingham_women$ShockIndex <- round(framingham_women$ShockIndex,2)


#Converting cholRisk to Readable values/levels
levels(framingham_women$cholRisk) <- c("Desireable","Borderline","High")


#restricting our results to filter out NAs 
framingham_women <- framingham_women %>% filter(cholRisk != "NA")

#using plotly and ggplot to graph the results 
plot_ly(framingham_women, y = ~ShockIndex, x = ~cholRisk, color = ~cholRisk, type = "box", jitter = 0.5)

#creating dummy variables for Chol levels
framingham_women <- framingham_women %>% dummy_cols()


paste("here are some summary statistics by groups - gender is women")
stats_by_cholRisk_women <- framingham_women %>% group_by(cholRisk) %>% summarise(counts = n(), mean = mean(ShockIndex), median(ShockIndex), sd(ShockIndex), min(ShockIndex),max(ShockIndex))
stats_by_cholRisk_women
