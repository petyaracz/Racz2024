# -- header -- #

set.seed(1337)

setwd('~/Github/Racz2024/')
library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)
library(gghalves)
library(patchwork)
library(lme4)

# -- fun -- #

cloudPlot2 = function(dat,pred,out){
  dat %>% 
    ggplot(aes({{pred}},{{out}})) +
    geom_hline(yintercept = 0.5) +
    geom_half_violin(side = 'r') +
    geom_half_boxplot(width = .1, side = 'r') +
    geom_half_point(width = .25, side = 'l') +
    theme_bw() +
    coord_flip()
}

# -- src -- #

source('analysis/esp_analysis/source_esp.R')

# -- best models -- #

esp_fit = glmer(esp_match ~ 1 + reg_rate + reg_dist * scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base), 
      data = esp, 
      family = binomial(link = 'logit'), 
      control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
                             ))

posttest_fit = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1 + 1|part_id) + (1|base), 
      data = posttest, 
      family = binomial(link = 'logit'), 
      control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
      ))

# -- predictions -- #

esp$pred = predict(esp_fit, type = 'response')
posttest$pred = predict(posttest_fit, type = 'response')

# -- camera ready -- #

esp$var = ifelse(esp$variation == 'lakok/lakom', 'levelling','vowel deletion')
posttest$var = ifelse(posttest$variation == 'lakok/lakom', 'levelling','vowel deletion')

# -- viz -- #

## baseline

b %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  mutate(ylab = ifelse(variation == 'lakok/lakom', 'levelling','vowel deletion') %>% 
           factor(levels = c('vowel deletion', 'levelling'))) %>% 
  cloudPlot2(ylab,log_odds)

## esp

esp %>% 
  group_by(part_id,reg_rate) %>% 
  summarise(mean = mean(pred)) %>% 
  cloudPlot2(reg_rate,mean) +
  ggtitle('Mean participant match with co-player') +
  ylab('Regularisation rate')

esp %>% 
  group_by(part_id,reg_dist) %>% 
  summarise(mean = mean(pred)) %>% 
  cloudPlot2(reg_dist,mean) +
  ggtitle('Mean participant match with co-player') +
  ylab('Regularisation distribution')

espIndex = function(dat){
  summarise(dat, mean = mean(pred), se = sd(pred)/sqrt(n()),upper = mean + se, lower = mean - se) %>%
    ggplot() +
    geom_hline(yintercept = .5, lty = 2) +
    geom_line(aes(i,mean)) +
    geom_line(aes(i,upper), lty = 3) +
    geom_line(aes(i,lower), lty = 3) +
    theme_few() +
    xlab('ESP trial number') +
    scale_x_continuous(breaks = c(1,seq(10,50,10),54)) +
    ylab('mean rate of matching co-player (predicted)')
}
  
esp %>% 
  group_by(i) %>% 
  espIndex +
  ggtitle('Mean trial match with co-player')

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic1.pdf', width = 8, height = 4)

p1 = esp %>% 
  group_by(i,reg_rate) %>% 
  espIndex +
  facet_wrap( ~ reg_rate, nrow = 1) +
  ylim(.4,.7)
p2 = esp %>% 
  group_by(i,reg_dist) %>% 
  espIndex +
  facet_wrap( ~ reg_dist, nrow = 1) +
  ylim(.4,.7) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
p3 = esp %>% 
  group_by(i,var) %>% 
  espIndex +
  facet_wrap( ~ var, nrow = 1) +
  ylim(.4,.7) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

p1 + p2 + p3 + plot_annotation(title = 'Mean trial match with co-player')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic2.pdf', width = 8, height = 4)

esp %>% 
  group_by(base,variation,reg_dist,baseline_log_odds) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean)) +
  geom_point(alpha = .1) +
  # geom_smooth(method = 'lm', colour = 'white') +
  theme_few() +
  facet_wrap( ~ reg_dist) +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('esp preference for variant 1 (p)') +
  ggtitle('Mean word match with co-player across\nword baseline log odds and\nregularisation distribution in ESP')

# chef's kiss

## posttest

posttest %>% 
  group_by(part_id,reg_rate) %>% 
  summarise(mean = mean(pred)) %>% 
  mutate(reg_rate = factor(reg_rate, levels = c('low','high'))) %>% 
  cloudPlot2(reg_rate,mean) +
  ggtitle('Mean participant preference\nfor variant 1 in posttest') +
  xlab('Regularisation rate') +
  ylab('mean predicted preference')

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic6.pdf', width = 4, height = 4)

posttest %>% 
  group_by(part_id,variation) %>% 
  summarise(mean = mean(pred)) %>% 
  cloudPlot2(variation,mean) +
  ggtitle('Mean participant variant 1 in posttest') +
  ylab('Regularisation rate')

posttest %>% 
  count(base,variation,reg_dist,reg_rate,baseline_log_odds,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE` + 1))) %>% 
  select(-`TRUE`,-`FALSE`) %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,colour = reg_dist)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_colorblind() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (log odds)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Mean word variant 1 across word baseline log odds and\nregularisation distribution in posttest')

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic7.pdf', width = 6, height = 4)

posttest %>% 
  count(base,variation,reg_dist,reg_rate,baseline_log_odds,picked_v1,var) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE` + 1))) %>% 
  select(-`TRUE`,-`FALSE`) %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,colour = var)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_viridis_d() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (log odds)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Mean word variant 1 across word baseline log odds and\nregularisation distribution in posttest') +
  facet_wrap( ~ reg_dist)

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic9.pdf', width = 8, height = 4)

posttest %>% 
  count(base,reg_dist,baseline_log_odds,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(
    posttest_log_odds = log((`TRUE` + 1)/(`FALSE` + 1)),
    diff = posttest_log_odds - baseline_log_odds
    ) %>% 
  select(-`TRUE`,-`FALSE`) %>% 
  ggplot(aes(reg_dist,diff)) +
  geom_boxplot()

# this one's got its heart in the right place but its unfortunately incomprehensible