setwd("~/D01591732748000314893/C/Users/Yuval/OneDrive - University of Haifa/data/Aristo")
library(tidyverse)
library(data.table)
library(stargazer)

clean_data <- readRDS("./data/clean_data.RDS")

clean_data[, treatment:=factor(treatment, levels = c("luck", "merit", "inheritance"))]

x_fig <- 20
y_fig <- 10

# Descriptive statistics --------------------------------------------------

t(clean_data[, .(mean_age=round(mean(age), 0), median_age=median(age), min_age=min(age), max_age=max(age), 
                 'High Income' = sum(income_label=="High Income", na.rm = T)/.N,
                 'Average Income' = sum(income_label=="Average Income", na.rm = T)/.N,
                 'Low Income' = sum(income_label=="Low Income", na.rm = T)/.N,
                 'Unreported Income' = sum(is.na(income_label))/.N,
                 'High Education' = sum(education %in% c(5,6), na.rm = T)/.N,
                 'Female' = sum(female)/.N,
                 Observations = .N), treatment])


# Figure 1 - Distribution of redistribution -------------------------------
clean_data[, .N, .(treatment, redistribution)][, .(redistribution, Prop = N/sum(N)), treatment] %>% 
  ggplot(mapping = aes(redistribution, Prop, fill = treatment)) + geom_bar(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) + labs(title = "Figure 1 - Distribution of redistribution")

ggsave(filename = "./exhibits/figure_1_dist_of_redist.jpg", device = "jpeg", units = "cm", width = x_fig, height = y_fig)


# Figure 2 = mean applied inequality --------------------------------------

mean_table <- clean_data[, t.test(applied_inequality, conf.level = 0.95, alternative = "two.sided", detailed = TRUE), treatment]
mean_table[, c("conf_low", "conf_high") := .(min(conf.int), max(conf.int)), treatment]
mean_table <- unique(mean_table, by="treatment")
mean_table[, treatment:=factor(treatment, levels = c("luck", "merit", "inheritance"))]

mean_table %>% ggplot(mapping = aes(treatment, estimate)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) + 
  labs(title = "Figure 2 - Average level of Applied Inequality in the Experiment")

ggsave(filename = "./exhibits/figure_2_applied_inequality.jpg", device = "jpeg", units = "cm", width = x_fig, height = y_fig)


# Figure 2 = mean applied inequality only high income--------------------------------------
mean_table <- clean_data[income_label =="High Income", t.test(applied_inequality, conf.level = 0.95, alternative = "two.sided", detailed = TRUE), treatment]
mean_table[, c("conf_low", "conf_high") := .(min(conf.int), max(conf.int)), treatment]
mean_table <- unique(mean_table, by="treatment")
mean_table[, treatment:=factor(treatment, levels = c("luck", "merit", "inheritance"))]

mean_table %>% ggplot(mapping = aes(treatment, estimate)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) + 
  labs(title = "Figure 2A - Average level of Applied Inequality in the Experiment - only High Income")

ggsave(filename = "./exhibits/figure_2A_applied_inequality_high_income.jpg", device = "jpeg", units = "cm", width = x_fig, height = y_fig)


# Regression - Applied Inequality (Table 4) -------------------------------

reg_app_inequality_no_controls <- lm(applied_inequality~merit + inheritance,data = clean_data)
summary(reg_app_inequality_no_controls)

reg_app_inequality_no_controls_clean <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = !is.na(income_label))
summary(reg_app_inequality_no_controls_clean)


reg_app_inequality <- lm(applied_inequality~merit + inheritance + age + `high education` + `high income` + female, data = clean_data)
summary(reg_app_inequality)

stargazer(reg_app_inequality_no_controls, reg_app_inequality_no_controls_clean, reg_app_inequality, type = "html", style = "AER",
          title = "Table 4, Regression Results: Dependent variable: Applied Inequality", out = "./exhibits/reg_app_inequality.html",
          omit.stat = c("f", "ser") )

# Regression - Applied Inequality (Table 4) high income sub groups -------------------------------

reg_app_inequality_no_controls_high_income <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = income_label=="High Income")
summary(reg_app_inequality_no_controls_high_income)

reg_app_inequality_high_income <- lm(applied_inequality~merit + inheritance + age + `high education` + female, 
                         data = clean_data, subset = income_label=="High Income")
summary(reg_app_inequality_high_income)

reg_app_inequality_no_controls_not_high_income <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = !is.na(income_label)&(income_label!="High Income"))
summary(reg_app_inequality_no_controls_not_high_income)


reg_app_inequality_not_high_income <- lm(applied_inequality~merit + inheritance + age + `high education` + female, 
                                         data = clean_data, subset = !is.na(income_label)&(income_label!="High Income"))
summary(reg_app_inequality_not_high_income)

stargazer(reg_app_inequality_no_controls_high_income, reg_app_inequality_high_income, 
          reg_app_inequality_no_controls_not_high_income, reg_app_inequality_not_high_income, type = "html", style = "AER",
          title = "Table 4, Regression Results: Dependent variable: Applied Inequality, high income sub groups", out = "./exhibits/reg_app_inequality_income_sub.html",
          omit.stat = c("f", "ser"), column.labels = c("High Income", "Not High Income"), column.separate = c(2, 2))


# Regression - Applied Inequality interactions (Table 5) -------------------------------
reg_app_inequality_gender <- lm(applied_inequality~merit*female + inheritance*female + age + `high education` + `high income` + female, data = clean_data)
summary(reg_app_inequality_gender)

reg_app_inequality_income <- lm(applied_inequality~merit*`high income` + inheritance*`high income` + age + `high education` + `high income` + female, data = clean_data)
summary(reg_app_inequality_income)

stargazer(reg_app_inequality_gender, reg_app_inequality_income, type = "html", style = "AER",
          title = "Table 5, Regression Results: interaction variables - female, income", out = "./exhibits/reg_app_inequality_interactions.html",
          omit.stat = c("f", "ser") )


# Figure 4 - Percentage of fairness types ---------------------------------
fairness_types <- data.table(type=character(), estimate=numeric(), conf_low = numeric(), conf_high = numeric())

Libertarians <- t.test(clean_data[treatment=="luck", redistribution==0])
fairness_types <- rbind(fairness_types, list("Libertarians", Libertarians$estimate[[1]], Libertarians$conf.int[[1]], Libertarians$conf.int[[2]]))

Egalitarians1 <- t.test(clean_data[treatment=="merit", redistribution==6])
Egalitarians2 <- t.test(clean_data[treatment=="inheritance", redistribution==6])
if(Egalitarians1$estimate<Egalitarians2$estimate) Egalitarians <- Egalitarians1 else Egalitarians <- Egalitarians2
fairness_types <- rbind(fairness_types, list("Egalitarians", Egalitarians$estimate[[1]], Egalitarians$conf.int[[1]], Egalitarians$conf.int[[2]]))

Meritocrats <- t.test(clean_data[treatment=="merit", redistribution<6], clean_data[treatment=="luck", redistribution<6])
fairness_types <- rbind(fairness_types, list("Meritocrats", Meritocrats$estimate[[1]]-Meritocrats$estimate[[2]], Meritocrats$conf.int[[1]], Meritocrats$conf.int[[2]]))

Strong_Meritocrats <- t.test(clean_data[treatment=="merit", redistribution<6], clean_data[treatment=="inheritance", redistribution<6])
fairness_types <- rbind(fairness_types, list("Strong Meritocrats", Strong_Meritocrats$estimate[[1]]-Strong_Meritocrats$estimate[[2]], Strong_Meritocrats$conf.int[[1]], Strong_Meritocrats$conf.int[[2]]))

Aristocrats <- t.test(clean_data[treatment=="inheritance", redistribution<6], clean_data[treatment=="luck", redistribution<6])
fairness_types <- rbind(fairness_types, list("Aristocrats", Aristocrats$estimate[[1]]-Aristocrats$estimate[[2]], Aristocrats$conf.int[[1]], Aristocrats$conf.int[[2]]))

fairness_types[, type:=factor(type, levels = c("Egalitarians", "Meritocrats", "Strong Meritocrats", "Aristocrats", "Libertarians"))]
fairness_types %>% ggplot(mapping = aes(type, estimate)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1) + 
  labs(title = "Figure 4 - Percentage of Fairness Types")

ggsave(filename = "./exhibits/figure_4_fairness_types.jpg", device = "jpeg", units = "cm", width = x_fig, height = y_fig)



# Robustness - response time ----------------------------------------------
# It does not seem like there is a significant part of the participants that answer in a non meaningful way.

clean_data %>% ggplot(mapping = aes(time_submit)) + geom_histogram(binwidth = 5) + geom_vline(xintercept = c(20), color = "red")
clean_data[time_submit<20]
clean_data[time_submit<20, mean(redistribution), treatment]
