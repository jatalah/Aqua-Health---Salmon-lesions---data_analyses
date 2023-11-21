library(tidyverse)
library(ggpubr)
library(readxl)

rm(list = ls())
source('theme_javier.R')

# final data  -------
rua_final <- 
  read_excel('data/RUA_tank trial_data.xlsx', 1, na = "NA") %>% 
  filter(Sampling == "Final") %>%
  pivot_longer(TotalSpots:SeverityScore)


frc_final <- 
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>% 
  filter(Sampling == "Final") %>%
  pivot_longer(TotalSpots:SeverityScore)

all_final <-
  bind_rows('On-farm' = rua_final,
            'Lab-trial' = frc_final,
            .id = 'Trial') %>%
  select(Trial, Treatment, name, value) %>%
  mutate(name = fct_recode(
    name,
    Severity = "SeverityScore",
    `Total spots` = "TotalSpots",
    `Total ulcers` = "TotalUlcer"
  ))

# Morts data --------
rua_mort <-
  read_excel('data/RUA_tank trial_data.xlsx', 1, na = "NA") %>%
  filter(Sampling == "Mortality") %>%
  pivot_longer(TotalSpots:SeverityScore)

frc_mort <-
  read_excel('data/FRC_tank trial_data.xlsx', 1) %>%
  filter(Sampling == "Mortality") %>%
  pivot_longer(TotalSpots:SeverityScore)

all_mort <-
  bind_rows('On-farm' = rua_mort,
            'Lab-trial' = frc_mort,
            .id = 'Trial') %>%
  select(Trial, Treatment, name, value) %>%
  mutate(name = fct_recode(
    name,
    Severity = "SeverityScore",
    `Total spots` = "TotalSpots",
    `Total ulcers` = "TotalUlcer"
  ))


all_d <- bind_rows(`Final sampling` = all_final, Mortalities = all_mort, .id = 'dataset')

# Plots -------------
ggplot(all_d, aes(fct_rev(name), value, fill = Treatment)) +
  geom_boxplot(alpha = .5,
               position = position_dodge(preserve = "single", width = 0.8)) +
  facet_grid(fct_rev(dataset) ~ fct_rev(Trial), scales = 'free_y') +
  labs(y = "Score", x = NULL, fill = NULL) +
  theme_javier(base_size = 8) +
  theme(legend.position = c(.9, .9))


ggsave(
  last_plot(),
  filename = 'figures/figure_5.svg',
  width = 89,
  height = 89,
  units = "mm",
  bg = 'white'
)

