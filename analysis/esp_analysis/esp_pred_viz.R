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

logOdds = function(dat,var){
  dat %>% 
    pivot_wider(names_from = {{var}}, values_from = n, values_fill = 0) %>% 
    mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1)))
} 

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

# -- coef -- #

broom.mixed::tidy(esp_fit, conf.int = T) %>% 
  write_tsv('analysis/esp_analysis/esp_fit.tsv')
broom.mixed::tidy(posttest_fit, conf.int = T) %>% 
  write_tsv('analysis/esp_analysis/posttest_fit.tsv')

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
  ggtitle('Mean trial match with co-player') +
  xlab('Matching trial number')

ggsave('analysis/esp_analysis/preds/pred1.png', width = 8, height = 4)

p1 = esp %>% 
  group_by(i,reg_rate) %>% 
  espIndex +
  facet_wrap( ~ reg_rate, nrow = 1) +
  ylim(.4,.7) +
  xlab('Matching trial number') +
  scale_x_continuous(breaks = c(1,25,54))
p2 = esp %>% 
  group_by(i,reg_dist) %>% 
  espIndex +
  facet_wrap( ~ reg_dist, nrow = 1) +
  ylim(.4,.7) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab('Matching trial number') +
  scale_x_continuous(breaks = c(1,25,54))
p3 = esp %>% 
  group_by(i,var) %>% 
  espIndex +
  facet_wrap( ~ var, nrow = 1) +
  ylim(.4,.7) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab('Matching trial number') +
  scale_x_continuous(breaks = c(1,25,54))

p1 + p2 + p3 + plot_annotation(title = 'Mean trial match with co-player')
ggsave('analysis/esp_analysis/preds/pred2.png', width = 8, height = 4)

p1 + theme(panel.background = element_rect(fill = 'lightgrey')) + p2 + theme(panel.background = element_rect(fill = 'lightgrey')) + p3 + theme(panel.background = element_rect(fill = 'lightgrey')) + plot_annotation(title = 'Mean trial match with co-player')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic2.pdf', width = 8, height = 4)

p1 + p2 + theme(panel.background = element_rect(fill = 'lightgrey')) + p3 + theme(panel.background = element_rect(fill = 'lightgrey')) + plot_annotation(title = 'Mean trial match with co-player')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic3.pdf', width = 8, height = 4)

p1 + theme(panel.background = element_rect(fill = 'lightgrey')) + p2 + p3 + theme(panel.background = element_rect(fill = 'lightgrey')) + plot_annotation(title = 'Mean trial match with co-player')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic4.pdf', width = 8, height = 4)

p1 + theme(panel.background = element_rect(fill = 'lightgrey')) + p2 + theme(panel.background = element_rect(fill = 'lightgrey')) + p3 + plot_annotation(title = 'Mean trial match with co-player')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic5.pdf', width = 8, height = 4)

esp %>% 
  group_by(base,variation,reg_dist,abs_baseline_log_odds_jitter) %>% 
  summarise(pred = mean(pred)) %>% 
  ggplot(aes(abs_baseline_log_odds_jitter,pred)) +
  geom_point(alpha = .1) +
  # geom_smooth(method = 'lm', colour = 'white') +
  theme_few() +
  facet_wrap( ~ reg_dist) +
  xlab('baseline preference for variant 1 or 2 (absolute log odds)') +
  ylab('predicted match with co-player') +
  ggtitle('Mean word match with co-player across word absolute baseline log odds\nand regularisation distribution in Matching task')
ggsave('analysis/esp_analysis/preds/pred3.png', width = 8, height = 4)

# chef's kiss

## posttest

posttest %>% 
  group_by(part_id,reg_rate) %>% 
  summarise(mean = mean(pred)) %>% 
  mutate(reg_rate = factor(reg_rate, levels = c('low','high'))) %>% 
  cloudPlot2(reg_rate,mean) +
  ggtitle('Mean predicted participant preference\nfor variant 1 in posttest') +
  xlab('Regularisation rate') +
  ylab('mean predicted preference')
ggsave('analysis/esp_analysis/preds/pred4.png', width = 5, height = 4)

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic6.pdf', width = 4, height = 4)

posttest %>% 
  group_by(part_id,variation) %>% 
  summarise(mean = mean(pred)) %>% 
  cloudPlot2(variation,mean) +
  ggtitle('Mean participant variant 1 in posttest') +
  ylab('Regularisation rate')

posttest %>% 
  group_by(base,variation,reg_dist,reg_rate,baseline_log_odds) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean,colour = reg_dist)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_colorblind() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (predicted)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Mean word variant 1 across word baseline log odds and\nregularisation distribution in posttest')
ggsave('analysis/esp_analysis/preds/pred5.png', width = 6, height = 4)
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic7.pdf', width = 6, height = 4)

posttest %>% 
  group_by(base,variation,reg_dist,reg_rate,baseline_log_odds,var) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean,colour = var)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_viridis_d() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (predicted)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Mean word variant 1 across word baseline log odds and\nregularisation distribution in posttest') +
  facet_wrap( ~ reg_dist)
ggsave('analysis/esp_analysis/preds/pred6.png', width = 6, height = 4)
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic9.pdf', width = 8, height = 4)

