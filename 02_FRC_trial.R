library(tidyverse)
library(readxl)
library(skimr)
library(ggpubr)
library(ggpmisc)
library(infer)
library(rstatix)

rm(list = ls())

# read data
dd <- read_excel('data/FRC_tank trial_data.xlsx', 1)

skim(dd)

# summarise 
dd %>%
  group_by(Sampling) %>%
  summarise(across(
    c(`Weight (g)`, `Length (mm)`),
    list(
      mean = \(x) mean(x, na.rm = T),
      se = \(x) sd(x, na.rm = T)/sqrt(n())
    )
  ))

# compare length and size by treatment baseline -------
dd %>%
  filter(Sampling == "Baseline") %>% 
  wilcox.test(data =., `Length (mm)` ~ Treatment)

dd %>%
  filter(Sampling == "Baseline") %>% 
  wilcox.test(data =., `Weight (g)` ~ Treatment)


ggplot(dd %>% filter(Sampling == "Baseline"),
       aes(Treatment, `Length (mm)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')

ggplot(dd %>% filter(Sampling == "Baseline"),
       aes(Treatment, `Weight (g)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')


# final data ---------
fcr_final <- dd %>% filter(Sampling == "Final")

fcr_final_long <- 
  fcr_final %>% 
  pivot_longer(TotalSpots:SeverityScore) 

ggplot(fcr_final_long, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot(alpha = .5) +
  coord_flip() +
  labs(y = "Score", x = NULL) +
  stat_compare_means(label.y = .8, label = "p.signif", size = 3, method = 'wilcox.test')


# survival analyses---------
library(ggsurvfit)
library(survival)
library(gtsummary)
library(tidycmprsk)

surv_d_frc <- 
  dd %>% 
  filter(Sampling != "Baseline") %>% 
  distinct(FishID, Tank, Date, Treatment, Sampling) %>% 
  mutate(start_date = dmy("16/02/2022"),
         time = as.duration(start_date %--% Date) / ddays(1),
         status = if_else(Sampling=="Mortality", 1, 0)) %>% 
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
coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc)

coxph(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc) %>% 
  tbl_regression(exp = T, show_single_row = "Treatment") 


# analyse proportion of morts by tank using chi-square 
morts_d <-
  dd %>% filter(Sampling == "Final") %>% 
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


# Lesions-----------
ggplot(fcr_final_long, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot(alpha =.45) +
  coord_flip() +
  labs(y = "Score", x = NULL) +
  stat_compare_means(label.y = .8, label = "p.signif", size = 3)


#same format as RUA plot - Lauren added
ggplot(fcr_final_long, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Score", x = NULL) +
  stat_compare_means(label.y = 3,
                     label = "p.format",
                     method = 'wilcox.test')


# prevalence plot
prev_d <-
  fcr_final %>%
  mutate(
    prevalence_ulcer = if_else(TotalUlcer > 0, 1, 0),
    prevalence_spots = if_else(TotalSpots > 0, 1, 0)
  ) %>%
  group_by(Treatment, Tank) %>%
  summarise(
    prevalence_ulcer = sum(prevalence_ulcer) / n(),
    prevalence_spots = sum(prevalence_spots) / n()
  )

ggplot(prev_d) +
  geom_col(aes(Treatment, prevalence_spots, fill = factor(Tank)), position = position_dodge()) +
  scale_y_continuous(labels = scales::label_percent()) +
  ggthemes::scale_fill_economist(name = "Tank")


# compare length and size by treatment final -------
dd %>%
  filter(Sampling == "Final") %>% 
  wilcox.test(data =., `Length (mm)` ~ Treatment)

dd %>%
  filter(Sampling == "Final") %>% 
  wilcox.test(data =., `Weight (g)` ~ Treatment)


ggplot(dd %>% filter(Sampling == "Final"),
       aes(Treatment, `Length (mm)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')

ggplot(dd %>% filter(Sampling == "Final"),
       aes(Treatment, `Weight (g)`)) +
  geom_boxplot() +
  stat_compare_means(method = 'wilcox.test')

# Bacterial swabs of lesions and normal skin (Lauren got from original "website")

bact_frc <- 
  fcr_final %>%
  mutate(across(`SKIN-BA`:`AK-MSSM`, 
                ~ fct_recode(.x,
                             `0` = "NG",
                             `1` = "1+",
                             `2` = "2+",
                             `3` = "3+"))) %>%
  mutate(across(`SKIN-BA`:`AK-MSSM`, as.character)) %>% 
  mutate(across(`SKIN-BA`:`AK-MSSM`, as.numeric)) %>% 
  drop_na(`SKIN-BA`:`AK-MSSM`) %>% 
  mutate()

bact_long_frc <- 
  bact_frc %>% 
  pivot_longer(`SKIN-BA`:`AK-MSSM`)

bact_long_skin_frc <- bact_long_frc %>% filter(str_detect(name, "SKIN"))

# ggplot(bact_long_skin_frc, aes(factor(SeverityScore), value)) +
#  stat_summary(fun.data = "mean_se", position = position_dodge(width = .5)) +
#  facet_wrap(~name) +
#  labs(y = "Bacterial score", x = "Severity Score") 

ggplot(bact_long_skin_frc, aes(factor(SeverityScore), value, color = name)) +
   # geom_point(fun.data = "mean_se", position = position_jitter(width = .1), alpha = .3) +
 stat_summary(fun.data = "mean_se", position = position_dodge(width = .5)) +
 labs(y = "Bacterial score", x = "Severity Score")

# symbols, not p values

ggplot(bact_long_frc, aes(name, value, color = Treatment)) +
  # geom_point(position = position_dodge(width = .5)) +
  stat_summary(fun.data = "mean_se", position = position_dodge(width = .5)) +
  coord_flip() +
  labs(x = "Bacteriological culture", y = "Score") +
  stat_compare_means(label.y = 2,
                     label = "p.signif",
                     size = 3) +
  facet_wrap( ~ BactoSampleType)

# p values shown on the plot

ggplot(bact_long_frc, aes(name, value, color = Treatment)) +
  # geom_point(position = position_dodge(width = .5)) +
  stat_summary(fun.data = "mean_se", position = position_dodge(width = .5)) +
  coord_flip() +
  labs(x = "Bacteriological culture", y = "Score") +
  stat_compare_means(label.y = 2,
                     label = "p.format",
                     size = 3) +
  facet_wrap( ~ BactoSampleType)
