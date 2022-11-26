# check esp predictions against baseline and esp data
# -- setup -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(magrittr)
library(glue)
library(patchwork)
library(lme4)
library(broom.mixed)

source('scripts/gcm/transcribe.R')

# -- data -- #

baseline = read_tsv('exp_data/baseline/baseline_tidy.tsv')
esp = read_tsv('exp_data/esp/esp_master.tsv')

baseline_gcm = read_tsv('resource/gcm/baseline_with_weights.tsv')
esp_gcm = read_tsv('resource/gcm/esp_with_weights.tsv')

# -- winnow and massage together -- #

lakok_b = filter(baseline, category == 'lakik') %>%  # of course heaven forbid I should be consistent with variable names
  select(file_name,my_list,word,my_choice,resp_is_first_variant)

lakok_b = baseline_gcm %>% 
  mutate(
    word = transcribe(word, 'double')
  ) %>% 
  right_join(lakok_b)

baseline_weights = lakok_b %>% 
  distinct(word,cat_1) %>% 
  rename(
    base = word, # I can't even
    gcm_baseline = cat_1
  )

# file name from ESP!
lakok_esp1 = esp %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,file_name,list_number)

# data from posttest!
lakok_esp2 = esp %>%  # for now! **dramatic**
  filter(trial_kind == 'posttest trial') %>% 
  select(part_id,reg_rate,reg_dist,picked_v1,base)

# now combine!
lakok_esp = left_join(lakok_esp1,lakok_esp2)

lakok_esp = esp_gcm %>% 
  mutate(
    base = transcribe(word, 'double')
  ) %>% 
  rename(gcm_esp = cat_1) %>% 
  select(-word) %>% 
  inner_join(lakok_esp) %>% 
  inner_join(baseline_weights)

# -- comparisons -- #

## baseline

lakok_b %>% 
  mutate(resp = as.double(resp_is_first_variant)) %>% 
  ggplot(aes(cat_1,resp)) +
  geom_jitter(width = .01, height = .25) +
  geom_smooth(method = glm, method.args= list(family="binomial")) +
  theme_bw()

lakok_b %>% 
  group_by(word,cat_1) %>% 
  summarise(resp = mean(resp_is_first_variant)) %>% 
  ggplot(aes(cat_1,resp,label = word)) +
  geom_label() +
  geom_smooth(alpha = .5) +
  theme_bw()

fit1 = glmer(resp_is_first_variant ~ cat_1 + (1|file_name) + (1|word), family = binomial, data = lakok_b)
tidy(fit1,conf.int = T)

## esp

lakok_esp_p_1 = lakok_esp %>% 
  group_by(base,gcm_baseline,gcm_esp,reg_rate,reg_dist) %>% 
  summarise(resp = mean(picked_v1)) %>% 
  pivot_longer(-c(base,reg_rate,reg_dist,resp), names_to = 'GCM', values_to = 'pred')

lakok_esp_p_1 %>% 
  ggplot(aes(pred,resp,colour = GCM)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ reg_rate + reg_dist)

fit2 = glmer(picked_v1 ~ gcm_esp + gcm_baseline + (1|file_name) + (1|base), family = binomial, data = lakok_esp)
tidy(fit2,conf.int = T)
fit2b = glmer(picked_v1 ~ gcm_baseline + (1|file_name) + (1|base), family = binomial, data = lakok_esp)
fit2c = glmer(picked_v1 ~ gcm_esp + (1|file_name) + (1|base), family = binomial, data = lakok_esp)
anova(fit2,fit2b)
anova(fit2,fit2c)

cor(lakok_esp$gcm_baseline,lakok_esp$gcm_esp)
