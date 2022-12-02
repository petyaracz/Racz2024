setwd('~/Github/Racz2024/')
library(tidyverse)

# d = read_tsv('exp_data/esp/esp_master_lakok.tsv')
d = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

# wrangling

d = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  right_join(d)

posttest = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )

#################################################
# esp

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(match = as.double(esp_match)) %>% 
  ggplot(aes(trial_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = glm, method.args= list(family="binomial")) +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(trial_index) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(trial_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(trial_index,reg_rate,reg_dist) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(trial_index, match, colour = reg_dist)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_rate) +
  geom_hline(yintercept = 0.5, lty = 2)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(part_id,reg_rate) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(reg_rate,match)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug()

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(reg_rate,match, colour = reg_dist)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug()

# posttest

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug()

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug()

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_dist,picked_v1,fill = reg_rate)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = c(.278,.722), lty = 3) +
  geom_rug()

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

ptsd = posttest %>% 
  count(base,reg_dist,reg_rate,baseline_log_odds,picked_v1) %>% 
  pivot_wider(id_cols = c(base,reg_dist,reg_rate,baseline_log_odds), names_from = picked_v1, values_from = n) %>% 
  mutate(
    post_test_log_odds = log((`TRUE`+1)/(`FALSE`+1))
  ) %>% 
  select(-`FALSE`,-`TRUE`) %>% 
  mutate(
    above_zero = case_when(
      baseline_log_odds >= 0 ~ 'above zero',
      baseline_log_odds < 0 ~ 'below zero',
      ),
    diff = baseline_log_odds - post_test_log_odds,
    diff_val = case_when(
      diff >= 0 ~ 'negative',
      diff < 0 ~ 'positive',
    )
         ) %>% 
  pivot_longer(-c(base,reg_dist,reg_rate,diff,diff_val,above_zero))

library(ggthemes)
ptsd %>% 
  ggplot(aes(name,value,group = base,colour = diff_val)) +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ reg_dist + reg_rate + above_zero, nrow = 2) +
  scale_colour_colorblind()

