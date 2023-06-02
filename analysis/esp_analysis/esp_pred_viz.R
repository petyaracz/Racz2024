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

# -- viz -- #

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

esp %>% 
  group_by(i) %>% 
  summarise(mean = mean(pred), se = sd(pred)/sqrt(21),upper = mean + se, lower = mean - se) %>% 
  ggplot() +
  geom_line(aes(i,mean)) +
  geom_line(aes(i,upper), lty = 3) +
  geom_line(aes(i,lower), lty = 3) +
  theme_few() +
  ggtitle('Mean trial match with co-player') +
  xlab('ESP trial number')

esp %>% 
  group_by(base,variation,reg_dist,abs_baseline_log_odds_jitter) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(abs_baseline_log_odds_jitter,mean)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm', colour = 'white') +
  theme_few() +
  facet_wrap( ~ reg_dist) +
  xlab('Absolute baseline log odds (jittered)') +
  ggtitle('Mean word match with co-player across\nword baseline log odds and\nregularisation distribution in ESP')

# chef's kiss

## posttest

posttest %>% 
  group_by(part_id,reg_rate) %>% 
  summarise(mean = mean(pred)) %>% 
  cloudPlot2(reg_rate,mean) +
  ggtitle('Mean participant variant 1 in posttest') +
  ylab('Regularisation rate')

posttest %>% 
  group_by(part_id,variation) %>% 
  summarise(mean = mean(pred)) %>% 
  cloudPlot2(variation,mean) +
  ggtitle('Mean participant variant 1 in posttest') +
  ylab('Regularisation rate')

posttest %>% 
  group_by(base,variation,reg_dist,baseline_log_odds_jitter) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds_jitter,mean,colour = reg_dist)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_colorblind() +
  xlab('Baseline log odds (jittered)') +
  ggtitle('Mean word variant 1 across\nword baseline log odds and\nregularisation distribution in posttest')

