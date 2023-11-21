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

# check data stats-------
skim(d)

# summarise data -----------
d %>%
  group_by(Sampling) %>%
  summarise(across(
    c(`Weight (g)`, `Length (mm)`),
    list(
      mean = \(x) mean(x, na.rm = T),
      se = \(x) sd(x, na.rm = T) / sqrt(n())
    )
  ))

# compare length and size by treatment baseline -------
base <- 
  d %>%
  filter(Sampling == "Baseline")

base %>% 
  group_by(Sampling) %>%
  wilcox.test(data =., `Length (mm)` ~ Treatment)

base %>%
  group_by(Sampling) %>%
  wilcox.test(data =., `Weight (g)` ~ Treatment)

# plots --------
ggplot(base,
       aes(Treatment, `Length (mm)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')

ggplot(base,
       aes(Treatment, `Weight (g)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')


# Mortality ------------ 
morts_d <- 
  full_join(
  d %>% filter(Sampling == "Mortality") %>% group_by(Treatment, Tank) %>% count(name = "morts"),
  d %>% filter(Sampling == "Final") %>% group_by(Treatment, Tank) %>% count(name = 'surv')
) %>% 
  mutate(n = morts + surv, 
         prop_mort = morts/n) %>% 
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
coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d)

coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d) %>% 
  tbl_regression(exp = T, show_single_row = "Treatment") 


mort_long <-
  d %>%
  filter(Sampling == "Mortality") %>%
  pivot_longer(TotalSpots:SeverityScore)
  
mort_long %>%
  group_by(Treatment, name) %>%
  summarise(across(value,
                   list(
                     median = \(x) median(x, na.rm = T),
                     Q1 = \(x) quantile(x, probs = 0.25),
                     Q3 = \(x) quantile(x, probs = 0.75)
                   ))) 


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
  filter(Sampling == "Final") 

final %>% 
  group_by(Sampling) %>%
  wilcox.test(data =., `Length (mm)` ~ Treatment)

final %>% 
  group_by(Sampling) %>%
  wilcox.test(data =., `Weight (g)` ~ Treatment)


ggplot(final,
       aes(Treatment, `Length (mm)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')

ggplot(final,
       aes(Treatment, `Weight (g)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')


# summary stats lesion, spots and ulcers-----------
final_long <- 
  final %>% 
  pivot_longer(TotalSpots:SeverityScore) 


final_long %>% 
  group_by(Treatment, name) %>% 
  summarise(across(value,
    list(
      median = \(x) median(x, na.rm = T),
      # se = \(x) sd(x, na.rm = T)/sqrt(n()),
      Q1 = \(x) quantile(x, probs = 0.25),
      Q3 = \(x) quantile(x, probs = 0.75)
    )
  ))


# differences in lesions  between treatments---
final_long %>%
  group_by(Tank, Treatment, name) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  group_by(name) %>%
  nest() %>%
  mutate(tests = map(data, ~ wilcox_test(value ~ Treatment, data = .x))) %>%
  select(tests) %>%
  unnest(cols = tests)

ggplot(final_long, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Score", x = NULL) +
  stat_compare_means(label.y = 3,
                     label = "p.format",
                     method = 'wilcox.test')