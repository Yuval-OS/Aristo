setwd("~/D01591732748000314893/C/Users/Yuval/OneDrive - University of Haifa/data/Aristo")
library(tidyverse)
library(data.table)


# Load and organize questionnaire data ------------------------------------

Sys.setlocale("LC_ALL", "Hebrew")
options(encoding ="UTF-8-BOM")

qcol_names <- readxl::read_excel("./data/raw data_qualtrics.xlsx", n_max = 0, col_names = TRUE)
questionnaire_data <- readxl::read_excel("./data/raw data_qualtrics.xlsx", col_names = names(qcol_names), skip = 2) %>% as.data.table()

setnames(questionnaire_data, old = c("חלוקה מזל", "חלוקה מריט", "חלוקה ירושה"), new = c("luck", "merit", "inheritance"))

short_data <- questionnaire_data[, .(Finished, `Time luck_Page Submit`, luck, `Time merit_Page Submit`, merit, `Time inheritance_Page Submit`, inheritance, SC0, userID)]

# make sure SC0 data is identical to text - confirmed
# short_data[, .N, .(inheritance, SC0)]

short_data[, c("luck", "merit", "inheritance"):=.(if_else(is.na(luck), FALSE, TRUE), if_else(is.na(merit), FALSE, TRUE), if_else(is.na(inheritance), FALSE, TRUE))]

options(encoding ="UTF-8")

short_data[luck==TRUE, c("time_submit", "treatment"):=.(`Time luck_Page Submit`, "luck")][merit==TRUE, c("time_submit", "treatment"):=.(`Time merit_Page Submit`, "merit")][inheritance==TRUE, c("time_submit", "treatment"):=.(`Time inheritance_Page Submit`, "inheritance")]

# hist(short_data$time_submit/60, breaks = 200)

short_data <- short_data[, -c("Time luck_Page Submit", "Time merit_Page Submit", "Time inheritance_Page Submit")]

#Keep only participants that completed the survey
short_data <- short_data[Finished=="True"]


# load and organize participants data -------------------------------------

participant_data <- readxl::read_excel("./data/participants_data.xlsx", col_names = TRUE, sheet = "Data") %>% as.data.table()

setnames(participant_data, old = c("col_1", "col_9", "col_10", "col_14", "col_22", "col_30"), new = c("sex", "education", "religion", "n_kids", "personal_income", "age"))

participant_data[, Female:=(sex==2)]

participant_data <- participant_data[, -c("ext_id", "start", "finish", "sex", "col_3", "col_6", "col_8", "col_12", "col_21", "col_42", "col_425", "col_2581")]

income_label_table <- data.table(personal_income = c(0, 1, 2, 3, 4, 5, 6), income_label = c(NA, "Low Income", "Low Income", "Average Income", "High Income", "High Income", NA))

participant_data <- merge(participant_data, income_label_table, by = "personal_income")

clean_data <- merge(short_data, participant_data, by.x = "userID", by.y = "GUID")

# save data ---------------------------------------------------------------

saveRDS(clean_data, "./data/clean_data.RDS")
write_csv(clean_data, "./data/clean_data.csv")
