library(tidyverse)
library(readxl)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(tidyr)
library(lubridate)

source("theme_javier.R")


panel_data <- 
  read_excel(
  "data/PANEL DATA.xlsx",
  col_types = c(
    "text",
    "text",
    "date",
    "text",
    "numeric",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  ),
  na = "NA"
) %>% 
  rename(Anemone = AnemoneScore) %>%
  mutate(Trial = fct_recode(Trial, `On-farm` = "RUA", `Lab trial` = "FRC"))

View(panel_data)


# figure_2------------
panel_long <-
  panel_data %>%
  pivot_longer(cols = c(LOF, Anemone))

ggplot(panel_long, aes(Dose, value, color = name)) +
  facet_wrap(~ fct_rev(Trial)) +
  labs(y = "Score", color = NULL) +
  stat_summary(fun.data = "mean_se",
               position = position_dodge(width = .5),
               size = .2) +
  theme_javier(base_size = 8) +
  #theme(legend.position = "bottom")



# score ANOVAS------------------------- 
bind_rows(
  LOF = anova(lm(LOF ~ Trial * Dose, data = panel_data)) %>% broom::tidy(),
  Anemone = anova(lm(Anemone ~ Trial * Dose, data = panel_data)) %>% broom::tidy(),
  .id = 'Score'
) %>% 
  write_csv('tables/anovas_scores.csv')

ggsave(
  last_plot(),
  filename = 'figures/figure_2.svg',
  width = 90,
  height = 40,
  bg = 'white',
  units = 'mm'
)

ggsave(
  last_plot(),
  filename = 'figures/figure_2.png',
  width = 90,
  height = 40,
  bg = 'white',
  units = 'mm'
)










# previous versions ------------

panel_data %>% 
  pivot_longer(cols = c(LOF, AnemoneScore)) %>% 
  ggplot(aes(Date, value, color = name, shape = Trial)) + 
  # facet_wrap(~Trial, scales = 'free_x') +
  stat_summary(fun.data = "mean_se", position = position_dodge(width = .5)) +
  theme_javier()



## Lauren version

panel_data %>% 
  pivot_longer(cols = c(LOF, AnemoneScore)) %>% 
  ggplot(aes(Date, value, color = name, shape = Trial)) + 
  # facet_wrap(~Trial, scales = 'free_x') +
  stat_summary(fun.data = "mean_se", position = position_dodge(width = .5)) +
  theme_javier() +
  labs(y = "Score") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(strip.text.y = element_text(size = 12))

# manuscript version

panel_data %>% 
  pivot_longer(cols = c(LOF, AnemoneScore)) %>% 
  ggplot(aes(Dose, value, color = name)) + 
  facet_wrap(~Trial) +
  stat_summary(fun.data = "mean_se", position = position_dodge(width = .2)) +
  theme_bw() +
  labs(y = "Score") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(strip.text.y = element_text(size = 12))
