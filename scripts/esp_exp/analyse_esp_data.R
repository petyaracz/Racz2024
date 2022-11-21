setwd('~/Github/Racz2024/')
library(tidyverse)

d = read_tsv('exp_data/esp/esp_master.tsv')

# baj lesz

d %>% 
  filter(picked_left) %>% 
  count(part_id,trial_kind) %>% 
  filter(n > 40)

d %>% 
  filter(trial_kind %in% c('esp trial', 'posttest trial')) %>% 
  mutate(
    missing = is.na(response_string)
  ) %>% 
  filter(missing) %>% 
  count(part_id,trial_kind)

# list counts

# when a list hits 7 take it out since we want 3*7 participants for each list
d %>% 
  pull(part_id) %>% 
  unique() %>% 
  length()

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist) %>% 
  count(list_number,reg_rate,reg_dist)

# esp

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(match = as.double(esp_match)) %>% 
  ggplot(aes(trial_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = glm, method.args= list(family="binomial")) +
  theme_bw()

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(trial_index) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(trial_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw()

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(trial_index,reg_rate,reg_dist) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(trial_index, match, colour = reg_dist)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_rate)

# posttest

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw()

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(esp_v1 = mean(esp_v1)) %>% 
  ggplot(aes(reg_rate,esp_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw()

# something something specific picks in esp?
d %>% 
  filter(trial_kind == 'esp trial') %>%
  ggplot(aes(trial_index,log_odds,colour = esp_v1)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ reg_rate + reg_dist)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(word = fct_reorder(base,log_odds)) %>% 
  ggplot(aes(word,log_odds,colour = esp_v1)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ reg_rate + reg_dist)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(word = fct_reorder(base, log_odds)) %>% 
  group_by(word,reg_rate,reg_dist) %>% 
  summarise(
    mean_part = mean(picked_v1),
    mean_robot = mean(esp_v1)
  ) %>% 
  ggplot(aes(mean_part,mean_robot)) +
  geom_jitter(width = .01, height = .05) +
  geom_smooth() +
  theme_bw() +
  facet_wrap(~ reg_rate + reg_dist)

