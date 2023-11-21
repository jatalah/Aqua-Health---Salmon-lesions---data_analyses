library(tidyverse)
library(readxl)
library(ggpubr)

# read soup data -------
soup <-
  read_excel('data/Bacteria.xlsx') %>%
  mutate(Dose = factor(Dose)) %>%
  pivot_longer(cols = c(TCBS:MSSM)) %>%
  mutate(name = fct_recode(
    name,
    `Tenacibaculum spp.` = "MSSM",
    `Vibrio spp.` = "TCBS"
  )) %>% 
  mutate(Trial = fct_recode(Trial, `On-farm` = "RUA", `Laboratory` = "FRC"))


# read water data -----
water <- 
  read_excel('data/Bacteria.xlsx', 2) %>% 
  mutate(Dose = factor(Dose)) %>%
  pivot_longer(cols = c(TCBS:MSSM)) %>%
  mutate(name = fct_recode(
    name,
    `Tenacibaculum spp.` = "MSSM",
    `Vibrio spp.` = "TCBS"
  )) %>% 
  mutate(Trial = fct_recode(Trial, `On-farm` = "RUA", `Laboratory` = "FRC")) %>% 
  filter(Trial== "On-farm")

figure_soup <- 
  ggplot(soup, aes(name,
                   value/1e6,
                   fill = Dose)) +
  geom_boxplot() +
  facet_wrap(~ fct_rev(Trial)) +
  labs(y = 'CFU/mL x 1e6', x = NULL) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Greens") +
  # theme(legend.position = c(.2,.8)) +
  theme_javier(base_size = 10)

figure_soup

# figure water-------
figure_water <- 
ggplot(water, aes(Treatment,
                 value,
                 fill = Dose)) +
  geom_boxplot(alpha = .7) +
  facet_wrap(~ name ) +
  labs(y = 'CFU/mL', x = NULL) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::comma) +
  # theme(legend.position = c(.2,.8)) + 
  theme_javier(base_size = 10)

figure_water

figure_3 <- ggarrange(figure_soup, figure_water, labels = 'AUTO', nrow = 2, legend = 'right')

figure_3

ggsave(
  figure_3,
  filename = 'figures/figure_3.svg',
  width = 180,
  height = 80,
  bg = 'white',
  units = 'mm'
)


# summary stats---------
soup %>%
  group_by(Dose, name, Trial) %>%
  summarise(across(
    value,
    list(
      min = min,
      median = median,
      max = max,
      Q1 = \(x) quantile(x, probs = 0.25),
      Q3 = \(x) quantile(x, probs = 0.75)
    )
  )) %>%
  write_csv('tables/summary_stats_soup.csv')

soup

water %>%
  group_by(Dose, Treatment, name) %>%
  summarise(across(
    value,
    list(
      min = min,
      median = median,
      max = max,
      Q1 = \(x) quantile(x, probs = 0.25),
      Q3 = \(x) quantile(x, probs = 0.75)
    )
  )) %>% 
  write_csv('tables/summary_stats_water.csv')

water

# stat----
# score ANOVAS------------------------- 

library(broom)

soup %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(anovas = map(data, ~anova(lm(value~Dose*Trial, data = .x))),
         tidied = map(anovas, tidy)) %>% 
  select(tidied) %>% 
  unnest(tidied) %>% 
  write_csv('tables/anovas_soup.csv')
  

water %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(anovas = map(data, ~anova(lm(value~Dose*Treatment, data = .x))),
         tidied = map(anovas, tidy)) %>% 
  select(tidied) %>% 
  unnest(tidied) %>% 
  write_csv('tables/anovas_water.csv')
