library(ggsurvfit)
library(survival)
library(gtsummary)
library(tidycmprsk)
library(ggpubr)
rm(list = ls())
source('theme_javier.R')


surv_d <- read_csv('data/surv_d_rua.csv')
surv_d_frc <- read_csv('data/surv_d_frc.csv')

# survival curves----
p1 <- 
survfit2(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d) %>% 
  ggsurvfit() +
  labs(
    x = "Exposure days",
    y = "Survival probability"
  ) +
  add_confidence_interval(type = 'ribbon', alpha = .1) +
  scale_y_continuous(limits = c(.5, 1)) +
  theme_javier(base_size = 7)
  
  
# add_risktable() 
p1

p2 <- 
  survfit2(Surv(time, status) ~ Treatment + cluster(Tank), data = surv_d_frc) %>% 
  ggsurvfit() +
  labs(
    x = "Exposure days",
    y = "Survival probability"
  ) +
  add_confidence_interval(type = 'ribbon', alpha = .1) +
  scale_y_continuous(limits = c(.5, 1)) +
  theme_javier(base_size = 7)


ggarrange(p1,
          p2,
          labels = 'AUTO',
          common.legend = T,label.x = -.005,
          legend = 'bottom', font.label = list(size = 7))
ggsave(
  last_plot(),
  filename = 'figures/figure_4.svg',
  width = 89,
  height = 40,
  units = "mm",
  bg = 'white'
)

