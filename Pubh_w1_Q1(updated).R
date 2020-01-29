#load necessary packages
library(tidyverse)
library(dplyr)
library(purrr)
library(tibble)
library(plotly)
library(magrittr)
library(corrplot)
library(data.table)
library(fastDummies)

#load in data for framingham.csv into a dataframe called framingham 
framingham <- read.csv("framingham_umn.csv",header = TRUE)

#convert framingham to data.table to speed up things, not useful in this case but why not
framingham <- data.table(framingham)

#filter for only women & whose sysBP was measured and only keeping the entries that are greater than 0. The new result
#is also saved as framingham_women
framingham_women <- framingham %>% filter(gender == "Female" & sysBP > 0 )

#Adding ShockIndex column CholRisk column and rounding ShockIndex to 2 sigfigs
framingham_women <- framingham_women %>% mutate(ShockIndex = heartRate/sysBP) %>% mutate(cholRisk = cut(framingham_women$totChol, breaks = c(0,200,240,600))) 
framingham_women$ShockIndex <- round(framingham_women$ShockIndex,2)

#Converting cholRisk to Readable values/levels - factors 
levels(framingham_women$cholRisk) <- c("Desireable","Borderline","High")

#restricting our results to filter out NAs 
#framingham_women <- framingham_women %>% filter(cholRisk != "NA")
framingham_women <- na.omit(framingham_women)

#using plotly and ggplot to graph the results 
plot_ly(framingham_women, y = ~ShockIndex, x = ~cholRisk, color = ~cholRisk, type = "box", jitter = 0.5)

#creating dummy variables for Chol levels
framingham_women <- framingham_women %>% dummy_cols()

paste("here are some summary statistics by groups - gender is women")
stats_by_cholRisk_women <- framingham_women %>% group_by(cholRisk) %>% summarise(counts = n(), mean = mean(ShockIndex), median(ShockIndex), sd(ShockIndex), min(ShockIndex),max(ShockIndex))
stats_by_cholRisk_women

#creating correlation matrix and removing gender and cholrisk
paste("Here is a correlation matrix") 
framingham_w_cor <- framingham_women %>% select(-c("gender","cholRisk","gender_Female","gender_Male"))
framingham_w_cor <- na.omit(framingham_w_cor)
framingham_corrplot <- corrplot(cor(framingham_w_cor[1:19]), order = "hclust", addrect = 3, insig = "blank")
framingham_corrplot

#creating a covariance matrix 
framingham_cov <- prcomp(framingham_w_cor, scale = FALSE)
framingham_cov

#completed

