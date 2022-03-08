setwd("~/D01591732748000314893/C/Users/Yuval/OneDrive - University of Haifa/data/Aristo")
library(tidyverse)
library(data.table)

clean_data <- readRDS("./data/clean_data.RDS")


# Descriptive statistics --------------------------------------------------

t(clean_data[, .(mean_age=round(mean(age), 0), median_age=median(age), min_age=min(age), max_age=max(age), 
                 'High Income' = sum(income_label=="High Income", na.rm = T)/.N,
                 'Average Income' = sum(income_label=="Average Income", na.rm = T)/.N,
                 'Low Income' = sum(income_label=="Low Income", na.rm = T)/.N,
                 'Unreported Income' = sum(is.na(income_label))/.N,
                 'High Education' = sum(education %in% c(5,6), na.rm = T)/.N,
                 'Female' = sum(Female)/.N,
                 Observations = .N), treatment])

