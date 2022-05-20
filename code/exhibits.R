setwd("~/D01591732748000314893/C/Users/Yuval/OneDrive - University of Haifa/data/Aristo")
library(tidyverse)
library(data.table)
library(stargazer)
library(gridExtra)
library(gt)
library(glue)

clean_data <- readRDS("./data/clean_data.RDS")

clean_data[, treatment:=factor(treatment, levels = c("luck", "merit", "inheritance"))]

x_fig <- 20
y_fig <- 10

x_paper <- 15
y_paper <- 15


theme_update(plot.title = element_text(hjust = 0.5))

# Descriptive statistics --------------------------------------------------

table1 <- clean_data[, .('Mean age'=round(mean(age), 0), 'Median age'=median(age), 'Min age'=min(age), 'Max age'=max(age), 
                 'High Income' = sum(income_label=="High Income", na.rm = T)/.N,
                 'Average Income' = sum(income_label=="Average Income", na.rm = T)/.N,
                 'Low Income' = sum(income_label=="Low Income", na.rm = T)/.N,
                 'Unreported Income' = sum(is.na(income_label))/.N,
                 'High Education' = sum(education %in% c(5,6), na.rm = T)/.N,
                 'Female' = sum(female)/.N,
                 Observations = .N), treatment]


table1a <- cbind(data.table(treatment = "All"), clean_data[, .('Mean age'=round(mean(age), 0), 'Median age'=median(age), 'Min age'=min(age), 'Max age'=max(age), 
               'High Income' = sum(income_label=="High Income", na.rm = T)/.N,
               'Average Income' = sum(income_label=="Average Income", na.rm = T)/.N,
               'Low Income' = sum(income_label=="Low Income", na.rm = T)/.N,
               'Unreported Income' = sum(is.na(income_label))/.N,
               'High Education' = sum(education %in% c(5,6), na.rm = T)/.N,
               'Female' = sum(female)/.N,
               Observations = .N)])

table1 <- rbind(table1a, table1)

table2 <- t(table1[,-c("treatment")])
colnames(table2) <- table1$treatment
table2 <- as.data.frame(table2)

setnames(table2, old = c("luck", "merit", "inheritance"), new = c("Luck", "Merit", "Inheritance"))

stargazer(table2, type = "html", title = "Experiment population", style = "AER", summary = F, 
          out = "./exhibits/Experiment_population.html", digits = 2)

stargazer(table2, type = "latex", title = "Experiment population", style = "AER", summary = F, 
          out = "./exhibits/Experiment_population.tex", digits = 2, label = "Experiment_population")

`# Figure XXX - Distribution of redistribution -------------------------------
clean_data[, choice:=factor(paste0("(", 12-redistribution, ",", redistribution, ")"), 
                            levels = c("(12,0)", "(10,2)", "(8,4)", "(6,6)", "(4,8)", "(2,10)", "(0,12)"))]

dist_4_plots <- function(specific_data, fig_name, yscale) {
  all <- specific_data[, .N, .(choice)][, .(choice, Prop = N/sum(N))] %>% 
    ggplot(mapping = aes(choice, Prop)) + geom_col() + scale_x_discrete(limits = c("(12,0)", "(10,2)", "(8,4)", "(6,6)", "(4,8)", "(2,10)", "(0,12)")) + 
    labs(title = "Pooled results", x = "Choice", y = "Proportion") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(0, yscale))
  
  sub_group_hist <- function(treat_name){
    fig <- specific_data[treatment==treat_name, .N, .(choice)][, .(choice, Prop = N/sum(N))] %>% 
      ggplot(mapping = aes(choice, Prop)) + geom_col() + scale_x_discrete(limits = c("(12,0)", "(10,2)", "(8,4)", "(6,6)", "(4,8)", "(2,10)", "(0,12)")) +
      labs(title = str_to_title(treat_name), x = "Choice", y = "Proportion") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, yscale))
    return(fig)
  }
  
  luck_fig <- sub_group_hist("luck")
  merit_fig <- sub_group_hist("merit")
  inheritance_fig <- sub_group_hist("inheritance")
  
  a <- grid.arrange(all, luck_fig, merit_fig, inheritance_fig)
  
  ggsave(plot = a, filename = paste0("./exhibits/figure_dist_of_choices_", fig_name, ".jpg"), device = "jpeg", units = "cm", width = x_paper, height = y_paper)
  
}

dist_4_plots(clean_data, "All", yscale = 0.55)
dist_4_plots(clean_data[`high income`==TRUE], "high_income", yscale = 0.8)
dist_4_plots(clean_data[`high income`==FALSE], "not_high_income", yscale = 0.8)


# Table XX1 & XX2 prop of choices ---------------------------------------------------------
specific_data <- clean_data
group_name <- "test"
title_name <- "daf"
prop_tables <- function(specific_data, title_name, group_name){
  all_choice_table <- specific_data[, .N, applied_inequality][, c("treatment", "Proportion"):=.("pooled", N/sum(N))][, N:=sum(N), treatment][,applied_inequality:=as.character(round(applied_inequality, digits = 2))]
  sub_group_choice_table <- specific_data[, .N, .(applied_inequality, treatment)][, Proportion:=N/sum(N), treatment][, N:=sum(N), treatment][,applied_inequality:=as.character(round(applied_inequality, digits = 2))]
  tablexx <- rbind(all_choice_table, sub_group_choice_table) %>% dcast(...~applied_inequality, value.var = "Proportion")
  tablexx <- tablexx[, .(treatment, `0`, `0.33`, `0.67`, `1`, N)]
  tablexx[, treatment:=factor(treatment, levels = c("luck", "merit", "inheritance", "pooled"))]
  setorder(tablexx, treatment)
  
  export_table <- gt(tablexx) %>% tab_header(title = title_name) %>% 
    fmt_percent(columns = c("0", "0.33", "0.67", "1"), decimals = 1)
  gtsave(export_table, filename = paste0("./exhibits/tablexx", group_name, ".html"))
  gtsave(export_table, filename = paste0("./exhibits/tablexx", group_name, ".tex"))
  
}

prop_tables(clean_data, title_name = "Applied Inequality Distribution", group_name = "1")
prop_tables(clean_data[`high income`==TRUE], title_name = "Applied Inequality Distribution - High Income", group_name = "2high")
prop_tables(clean_data[`high income`==FALSE], title_name = "Applied Inequality Distribution - not High Income", group_name = "2notHigh")


# count_table <- clean_data[, .N, .(applied_inequality, treatment)]
# tablexx1_count <- xtabs(N~treatment + applied_inequality, data = count_table)
# chisq.test(tablexx1_count)
# tablexx1_prop <- prop.table(tablexx1_count)

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

#reg_app_inequality_no_controls
m1  <- lm(applied_inequality~merit + inheritance,data = clean_data)
summary(m1)

#reg_app_inequality_no_controls_clean
m2 <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = !is.na(income_label))
summary(m2)

#reg_app_inequality
m3 <- lm(formula = applied_inequality~merit + inheritance + age + `high education` + `high income` + female, data = clean_data)
summary(reg_app_inequality)

stargazer(m1, m2, m3, type = "html", style = "AER",
          title = "Treatments Effect on Applied Inequality", out = "./exhibits/reg_app_inequality.html",
          omit.stat = c("f", "ser"), dep.var.labels="")

stargazer(m1, m2, m3, type = "latex", style = "AER",
          title = "Treatments Effect on Applied Inequality", out = "./exhibits/reg_app_inequality.tex",
          omit.stat = c("f", "ser"), label = "reg_app_inequality", dep.var.labels="")


# Regression - Applied Inequality (Table 4) high income sub groups -------------------------------

reg_app_inequality_no_controls_high_income <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = income_label=="High Income")
summary(reg_app_inequality_no_controls_high_income)

reg_app_inequality_high_income <- lm(applied_inequality~merit + inheritance + age + `high education` + female, 
                         data = clean_data, subset = income_label=="High Income")
summary(reg_app_inequality_high_income)

reg_app_inequality_no_controls_average_income <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = !is.na(income_label)&(income_label=="Average Income"))
summary(reg_app_inequality_no_controls_average_income)


reg_app_inequality_average_income <- lm(applied_inequality~merit + inheritance + age + `high education` + female, 
                                         data = clean_data, subset = !is.na(income_label)&(income_label=="Average Income"))
summary(reg_app_inequality_average_income)

reg_app_inequality_no_controls_low_income <- lm(applied_inequality~merit + inheritance,data = clean_data, subset = !is.na(income_label)&(income_label=="Low Income"))
summary(reg_app_inequality_no_controls_low_income)

reg_app_inequality_low_income <- lm(applied_inequality~merit + inheritance + age + `high education` + female, 
                                        data = clean_data, subset = !is.na(income_label)&(income_label=="Low Income"))
summary(reg_app_inequality_low_income)

stargazer(reg_app_inequality_no_controls_high_income, reg_app_inequality_high_income, 
          reg_app_inequality_no_controls_average_income, reg_app_inequality_average_income, reg_app_inequality_no_controls_low_income,
          reg_app_inequality_low_income, type = "html", style = "AER",
          title = "Table 4, Regression Results: Dependent variable: Applied Inequality, income sub groups", out = "./exhibits/reg_app_inequality_income_sub.html",
          omit.stat = c("f", "ser"), column.labels = c("High Income", "Average Income", "Low Income"), column.separate = c(2, 2, 2))


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

fairness_types[conf_low<0, conf_low:=0]

fairness_types_US <- data.table(type = c("Egalitarians", "Libertarians", "Meritocrats"), estimate = c(0.153, 0.294, 0.375))

fairness_types[, type:=factor(type, levels = c("Egalitarians", "Meritocrats", "Strong Meritocrats", "Aristocrats", "Libertarians"))]

fairness_types %>% ggplot(mapping = aes(type, estimate)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1) + 
  labs(y = "Proportion") + theme_bw() + theme(text=element_text(size=15), axis.title.x = element_blank()) + geom_col(data = fairness_types_US, mapping = aes(type, estimate), alpha=0 ,color="red", size = 1)

ggsave(filename = "./exhibits/figure_4_fairness_types.jpg", device = "jpeg", units = "cm", width = x_fig, height = y_fig)



# Robustness - response time ----------------------------------------------
# It does not seem like there is a significant part of the participants that answer in a non meaningful way.

clean_data %>% ggplot(mapping = aes(time_submit)) + geom_histogram(binwidth = 5) + geom_vline(xintercept = c(20), color = "red")
clean_data[time_submit<20]
clean_data[time_submit<20, mean(redistribution), treatment]

clean_data[time_submit<150] %>% ggplot(mapping = aes(time_submit, redistribution, color=treatment)) + geom_jitter(width = 0.01, height = 0.2) + geom_smooth(se=F, )

clean_data[time_submit<150] %>% ggplot(mapping = aes(time_submit, color = treatment)) + geom_boxplot()
