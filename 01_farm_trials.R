library(tidyverse)
library(readxl)
library(skimr)
library(ggpubr)
library(ggpmisc)
library(infer)
library(rstatix)

rm(list = ls())

# read data -----------
d <- read_excel('data/RUA_tank trial_data.xlsx', 1, na = "NA")
source('theme_javier.R')
theme_set(theme_javier(base_size = 9))


# baseline data compare length and size by treatment -------
base <-
  d %>% 
  filter(Sampling == "Baseline") %>% 
  pivot_longer(cols = c(`Length (mm)`, `Weight (g)`)) 

base %>% 
  group_by(name) %>% 
  get_summary_stats(value)

base %>% 
  group_by(name) %>% 
  wilcox_test(value ~ Treatment)

# plots --------
ggplot(base, aes(Treatment, value, fill = Treatment)) +
  geom_boxplot(alpha = .7) +
  facet_wrap(~name, scales = 'free') +
  stat_compare_means(method = 'wilcox.test')


# Mortality ------------ 
morts_d <-
  full_join(
    d %>% filter(Sampling == "Mortality") %>% group_by(Treatment, Tank) %>% count(name = "morts"),
    d %>% filter(Sampling == "Final") %>% group_by(Treatment, Tank) %>% count(name = 'surv')
  ) %>%
  mutate(n = morts + surv,
         prop_mort = morts / n) %>%
  ungroup()

morts_d

# # mortality 4-tanks test for equality of proportions
# prop.test(morts_d$morts, morts_d$n)
# 
# # mortality by treatment test for equality of proportions
# prop.test(x = c(4, 5), n = c(59, 56))

## survival analysis------------------
library(ggsurvfit)
library(survival)
library(gtsummary)
library(tidycmprsk)

surv_d <- 
  d %>% 
  filter(Sampling != "Baseline") %>% 
  distinct(FishID, Tank, Date, Treatment, Sampling) %>% 
  mutate(start_date = dmy("20/01/2022"),
         time = as.duration(start_date %--% Date) / ddays(1),
         status = if_else(Sampling=="Mortality", 1, 0)) %>% 
  select(FishID, Tank, Treatment, time , status) %>% 
  mutate(Treatment = fct_relevel(Treatment, "Control"))

write_csv(surv_d, 'data/surv_d_rua.csv')

# survival curves----
survfit2(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d) %>% 
  ggsurvfit() +
  labs(
    x = "Exposure days",
    y = "Survival probability"
  ) +
  add_confidence_interval() +
  add_risktable()


# summary of survival model 
survfit(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d) %>% 
  tbl_survfit(
    times = 40,
    label_header = "**40-day survival (95% CI)**",
  )

# The Cox regression model is a semi-parametric model that can be used to fit regression models that have survival outcomes. # We can fit regression models for survival data using the coxph() function from the {survival} package
cox_rua <- coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d) 
  
summary(cox_rua)

cox_frc %>% 
broom::tidy(conf.int = T, exponentiate = T) %>%  
  write_csv('tables/cox_regression_survival_RUA.csv')

coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d) %>% 
  tbl_regression(exp = T, show_single_row = "Treatment") 

# Mortality data -------
mort_long <-
  d %>%
  filter(Sampling == "Mortality") %>%
  pivot_longer(TotalSpots:SeverityScore)
  
mort_long %>%
  group_by(Treatment, name) %>% 
  get_summary_stats(value)

ggplot(mort_long, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot(alpha = .5) +
  coord_flip() +
  labs(y = "Score", x = NULL) +
  stat_compare_means(label.y = 15,
                     label = "p.format",
                     method = 'wilcox.test')

# compare length and size by treatment final -------
final <- 
  d %>% 
  filter(Sampling == "Final") %>% 
  pivot_longer(cols = c(`Length (mm)`, `Weight (g)`, TotalSpots:SeverityScore)) 

# summary stats --------
final %>% 
  group_by(name) %>% 
  get_summary_stats(value)

write_csv(.Last.value, 'tables/RUA_summary_stats_lesions.csv')

# Wilcox - test ------------
final %>% 
  group_by(name) %>%
  wilcox_test(value ~ Treatment)

write_csv(.Last.value, 'tables/RUA_wilcox_tests_lesions.csv')

ggplot(final,
       aes(Treatment, value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free')