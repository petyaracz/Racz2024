#############################################
#############################################
# compare exp2 and exp3
#############################################
#############################################

# -- header -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(glue)
library(magrittr)
library(patchwork)
library(lme4)
library(mgcv)
library(performance)
library(broom.mixed)
library(parallel)

# -- source -- #

source('analysis/esp_analysis/source_esp.R')

# -- comparisons -- #

besp = esp %>% 
  filter(reg_dist == 'reversed', reg_rate == 'low') %>% 
  mutate(experiment = 'exp2') %>% 
  select(part_id,base,i,esp_match,experiment,variation)
bposttest = posttest %>% 
  filter(reg_dist == 'reversed', reg_rate == 'low') %>% 
  mutate(experiment = 'exp2') %>% 
  select(part_id,base,i,picked_v1,experiment,variation)

sesp2 = sesp %>%
  mutate(experiment = 'exp3') %>% 
  select(part_id,base,i,esp_match,experiment,variation)
sposttest2 = sposttests %>%
  filter(trial_kind == 'posttest trial') %>% 
  mutate(experiment = 'exp3') %>% 
  select(part_id,base,i,picked_v1,experiment,variation)

esps = bind_rows(besp,sesp2)
posttests = bind_rows(bposttest,sposttest2)

# -- models -- #

## esp learning

fit1 = glmer(esp_match ~ 1 + scale(i) * variation * experiment + (1|part_id) + (1|base), family = binomial, data = esps)
fit2 = glmer(esp_match ~ 1 + scale(i) + variation * experiment + (1|part_id) + (1|base), family = binomial, data = esps)
fit3 = glmer(esp_match ~ 1 + scale(i) * experiment + variation + (1|part_id) + (1|base), family = binomial, data = esps)
fit4 = glmer(esp_match ~ 1 + scale(i) + experiment + variation + (1|part_id) + (1|base), family = binomial, data = esps)
fit5 = glmer(esp_match ~ 1 + scale(i) + experiment + (1|part_id) + (1|base), family = binomial, data = esps)

compare_performance(fit1,fit2,fit3,fit4, metrics = 'common')
test_performance(fit3,fit4)
tidy(fit4)
test_likelihoodratio(fit4,fit5)

## posttest learning


fit6 = glmer(picked_v1 ~ 1 + variation * experiment + (1|part_id) + (1|base), family = binomial, data = posttests)
fit7 = glmer(picked_v1 ~ 1 + variation + experiment + (1|part_id) + (1|base), family = binomial, data = posttests)
fit8 = glmer(picked_v1 ~ 1 + variation + (1|part_id) + (1|base), family = binomial, data = posttests)

compare_performance(fit6,fit7,fit8, metrics = 'common')
test_performance(fit7,fit8)
tidy(fit4)
test_likelihoodratio(fit7,fit8)

