setwd('~/Github/Racz2024/')
library(tidyverse)
library(ggthemes)

dl = read_tsv('exp_data/esp/esp_master_lakok.tsv')
dcs = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

# wrangling

dl$part_yob = NULL
dcs$part_yob = NULL

d = bind_rows(dcs,dl)
d = dl

d = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  right_join(d) %>% 
  # mutate(
    # part_yob = str_extract(part_yob, '^[0-9]{4}')
  # ) %>% 
  arrange(trial_index) %>% 
  group_by(part_id,trial_kind) %>% 
  mutate(kind_index = 1:n()) %>% 
  ungroup() %>% 
  mutate(
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )
  
posttest = d %>% 
  filter(trial_kind == 'posttest trial')

#################################################
# times

d %>% 
  group_by(part_id) %>% 
  filter(trial_index %in% c(min(trial_index),max(trial_index))) %>% 
  select(part_id,trial_index,time_elapsed) %>% 
  pivot_wider(part_id, names_from = trial_index, values_from = time_elapsed) %>% 
  mutate(how_long = (`173` - `11`) / 60000) %>% 
  ggplot(aes(how_long)) +
  geom_histogram() +
  theme_bw()

#################################################
# esp

d %>% 
  distinct(base,baseline_log_odds) %>% 
  ggplot(aes(baseline_log_odds)) +
  geom_histogram() +
  theme_bw()

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(match = as.double(esp_match)) %>% 
  ggplot(aes(kind_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = glm, method.args= list(family="binomial")) +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2)

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  group_by(kind_index) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(kind_index, match)) +
  geom_point(alpha = .5) +
  # geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab('matches robot') +
  xlab('trial index 1:54') +
  ggtitle('agreement with robot in ESP phase (phase 1)')

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(kind_index,reg_rate,reg_dist) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(kind_index, match, colour = reg_dist)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_rate) +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab('matches robot') +
  xlab('trial index 1:54') +
  ggtitle('agreement with robot across conditions\nin ESP phase (phase 1)')

# posttest

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug() +
  labs(fill = 'robot\nresponse\ndistribution') +
  xlab('robot response 1 rate') +
  ylab('picked response 1') 

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist,picked_v1) %>% 
  summarise(mean_rt = mean(rt)) %>% 
  ggplot(aes(reg_dist,mean_rt,fill = picked_v1)) +
  geom_boxplot() +
  theme_bw() +
  geom_rug() +
  facet_wrap( ~ reg_rate)
  
d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate) %>% 
  summarise(mean_rt = mean(rt)) %>% 
  ggplot(aes(reg_rate,mean_rt)) +
  geom_boxplot() +
  theme_bw() +
  geom_rug()

# posttest vs baseline

d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  count(base,baseline_log_odds,reg_dist,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = log((`TRUE`+1)/(`FALSE`+1))) %>% 
  ggplot(aes(baseline_log_odds,post_test_log_odds,colour = reg_dist)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm')
