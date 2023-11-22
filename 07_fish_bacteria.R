library(tidyverse)
library(ggpubr)
library(readxl)
library(skimr)
library(ggpmisc)
library(infer)
library(rstatix)

rm(list = ls())
source('theme_javier.R')

# bacterial data preparation-----------
prep_fuction <- function(path) {
  read_excel(path, 1, na = "NA") %>%
    filter(Sampling == "Final") %>%
    group_by(Treatment, BactoSampleType) %>%
    mutate(across(
      c(`SKIN-MSSM`, `SKIN-TCBS`, `AK-MSSM`, `AK-TCBS`),
      ~ fct_recode(
        .x,
        `0` = "NG",
        `1` = "1+",
        `2` = "2+",
        `3` = "3+"
      )
    ), .keep = 'used') %>%
    mutate(across(`SKIN-TCBS`:`AK-MSSM`, as.character)) %>%
    mutate(across(`SKIN-TCBS`:`AK-MSSM`, as.numeric)) %>%
    drop_na(`SKIN-TCBS`:`AK-MSSM`) %>%
    mutate() %>%
    pivot_longer(`SKIN-TCBS`:`AK-MSSM`) %>%
    mutate(
      Tissue = if_else(str_detect(name, "AK"), 'Kidney', 'Skin'),
      Test = if_else(str_detect(name, "MSSM"), 'Tenacibaculum spp.', 'Vibrio spp.'),
      BactoSampleType = str_to_sentence(BactoSampleType)
    )
}

bact_rua <- prep_fuction('data/RUA_tank trial_data.xlsx')
bact_frc <- prep_fuction('data/FRC_tank trial_data.xlsx')

# summary stats------
bind_rows(
  RUA =
    bact_rua %>%
    group_by(Test, Tissue, BactoSampleType , Treatment) %>%
    get_summary_stats(value),
  FRC = bact_frc %>%
    group_by(Test, Tissue, BactoSampleType , Treatment) %>%
    get_summary_stats(value),
  .id = "Trial"
) %>% 
  write_csv('tables/summary_stats_fish_bacteria.csv')

# Wilcox - test --------------
bind_rows(
  RUA =
    bact_rua %>%
    filter(Test != "Kidney") %>%
    group_by(Test, BactoSampleType) %>%
    wilcox_test(value ~ Treatment),
  FRC =
    bact_frc %>%
    group_by(Test, Tissue, BactoSampleType) %>%
    wilcox_test(value ~ Treatment),
  .id = 'Trial'
) %>% 
  write_csv('tables/wilcox_tests_fish_bacteria.csv')


# Figure 6 - bacterial load plot ------------
plot_func <- function(data) {
    ggplot(data, aes(Test, value, fill = Treatment)) +
      labs(x = NULL, y = "Score", fill = NULL) +
      geom_boxplot(alpha = .7, linewidth = .25) +
      facet_grid(Tissue ~ BactoSampleType) +
      theme_javier(base_size = 9)  +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 10)
      )
  }

b1 <- plot_func(bact_rua)
b2 <- plot_func(bact_frc)

# put plots together------------  
ggarrange(b1,
          b2,
          labels = 'AUTO',
          common.legend = T,
          legend = 'bottom', 
          font.label = list(size = 8), nrow = 2)

# save plots -----
ggsave(
  last_plot(),
  filename = 'figures/figure_6.svg',
  width = 89,
  height = 120,
  units = "mm",
  bg = 'white'
)

# with p values-----
ggarrange(b1 + stat_compare_means(label.y = 2.5, label = "p.format", size = 3),
          b2 + stat_compare_means(label.y = 2.5, label = "p.format", size = 3),
          labels = 'AUTO',
          common.legend = T,
          legend = 'bottom', 
          font.label = list(size = 8), nrow = 2)

ggsave(
  last_plot(),
  filename = 'figures/figure_6_with_p_values.svg',
  width = 89,
  height = 120,
  units = "mm",
  bg = 'white'
)
