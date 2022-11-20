setwd('~/Github/Racz2024/')
library(tidyverse)

d = read_tsv('exp_data/esp/esp_master.tsv')

d %>% 
  filter(picked_left) %>% 
  count(part_id,trial_kind) %>% 
  filter(n > 40)

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist) %>% 
  count(list_number,reg_rate,reg_dist)

d %>% 
  filter(trial_kind %in% c('esp trial', 'posttest trial')) %>% 
  mutate(
    missing = is.na(response_string)
  ) %>% 
  filter(missing) %>% 
  count(part_id,trial_kind)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(match = as.double(esp_match)) %>% 
  ggplot(aes(trial_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = glm, method.args= list(family="binomial")) +
  theme_bw()

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(trial_index,reg_rate,reg_dist) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(trial_index, match, colour = reg_dist)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth() +
  theme_bw() +
  facet_wrap( ~ reg_rate)

# ??
d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw()
