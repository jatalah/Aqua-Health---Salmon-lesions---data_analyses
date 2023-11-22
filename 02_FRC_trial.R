library(tidyverse)
library(readxl)
library(ggpubr)
library(rstatix)

rm(list = ls())

# read data
frc_base <- 
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>%  
  pivot_longer(cols = c(`Length (mm)`, `Weight (g)`)) %>% 
  filter(Sampling == "Baseline") 


# summarise 
frc_base %>%
  group_by(name) %>% 
  get_summary_stats(value)

# compare length and size by treatment baseline -------
frc_base %>%
  group_by(name) %>% 
  wilcox_test(value ~ Treatment)

ggplot(frc_base,
       aes(Treatment, value)) +
  geom_boxplot() +
  facet_wrap(~name, scale = 'free')


# final data ---------
fcr_final <- 
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>%   
  filter(Sampling == "Final") %>%  
  pivot_longer(c(`Length (mm)`, `Weight (g)`, TotalSpots:SeverityScore))


# stats ----
fcr_final %>% 
  group_by(name) %>% 
  get_summary_stats(value)

write_csv(.Last.value, 'tables/frc_summary_stats_lesions.csv')

# Wilcox values---
fcr_final %>% 
  group_by(name) %>% 
  wilcox_test(value~Treatment)

write_csv(.Last.value, 'tables/frc_wilcox_tests_lesions.csv')

# boxplots------
ggplot(fcr_final, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~name, scales = 'free') +
  labs(y = "Score", x = NULL)

# survival analyses---------
library(ggsurvfit)
library(survival)
library(gtsummary)
library(tidycmprsk)

surv_d_frc <-
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>%
  filter(Sampling != "Baseline") %>%
  distinct(FishID, Tank, Date, Treatment, Sampling) %>%
  mutate(
    start_date = dmy("16/02/2022"),
    time = as.duration(start_date %--% Date) / ddays(1),
    status = if_else(Sampling == "Mortality", 1, 0)
  ) %>%
  select(FishID, Tank, Treatment, time , status) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control"))

write_csv(surv_d_frc, 'data/surv_d_frc.csv')

# survival curves----
survfit2(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc) %>% 
  ggsurvfit() +
  labs(
    x = "Exposure days",
    y = "Survival probability"
  ) +
  add_confidence_interval() +
  add_risktable()

# summary of survival model 
survfit(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc) %>% 
  tbl_survfit(
    times = 28,
    label_header = "**28-day survival (95% CI)**",
  )

# The Cox regression model is a semi-parametric model that can be used to fit regression models that have survival outcomes. # We can fit regression models for survival data using the coxph() function from the {survival} package
cox_frc <-   coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc)
summary(cox_frc)


cox_frc %>% 
broom::tidy(conf.int = T, exponentiate = T)


write_csv(.Last.value, 'tables/cox_regression_survival_frc.csv')

coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc) %>% 
  tbl_regression(exp = T, show_single_row = "Treatment") 


# analyse proportion of morts by tank using chi-square --------------
morts_d <-
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>% 
  filter(Sampling == "Final") %>% 
  group_by(Treatment, Tank) %>% 
  count(name = 'surv') %>%
  mutate(n = 10,
         morts =   n - surv,
         prop_mort = morts / n) %>%
  ungroup()

morts_d %>% 
  group_by(Treatment) %>% 
  summarise(sum = sum(morts))
  
# mortality by treatment test for equality of proportions
prop.test(x = c(8, 0), n = c(40,40))


# lesions prevalence plot--------------
prev_d <-
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>% 
  filter(Sampling == "Final") %>%
  mutate(
    prevalence_ulcer = if_else(TotalUlcer > 0, 1, 0),
    prevalence_spots = if_else(TotalSpots > 0, 1, 0)
  ) %>%
  group_by(Treatment, Tank) %>%
  summarise(
    prevalence_ulcer = sum(prevalence_ulcer) / n(),
    prevalence_spots = sum(prevalence_spots) / n()
  ) %>% 
  ggplot() +
  geom_col(aes(Treatment, prevalence_spots, fill = factor(Tank)), position = position_dodge()) +
  scale_y_continuous(labels = scales::label_percent()) +
  ggthemes::scale_fill_economist(name = "Tank")
