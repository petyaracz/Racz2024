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
# d = dl

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
  group_by(part_id,variation) %>% 
  filter(trial_index %in% c(min(trial_index),max(trial_index))) %>%
  select(part_id,trial_index,time_elapsed,variation) %>% 
  pivot_wider(c(part_id,variation), names_from = trial_index, values_from = time_elapsed) %>% 
  mutate(how_long = (`173` - `11`) / 60000) %>% 
  ggplot(aes(how_long)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ variation)

#################################################
# esp

d %>% 
  distinct(base,baseline_log_odds,variation) %>% 
  ggplot(aes(baseline_log_odds)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ variation)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  mutate(match = as.double(esp_match)) %>% 
  ggplot(aes(kind_index, match)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = glm, method.args= list(family="binomial")) +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  facet_wrap( ~ variation)

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  group_by(kind_index,variation) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(kind_index, match)) +
  geom_point(alpha = .5) +
  # geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab('matches robot') +
  xlab('trial index 1:54') +
  ggtitle('agreement with robot in ESP phase (phase 1)') +
  facet_wrap( ~ variation)

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(kind_index,reg_rate,reg_dist,variation) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(kind_index, match, colour = reg_dist)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_rate) +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab('matches robot') +
  xlab('trial index 1:54') +
  ggtitle('agreement with robot across conditions\nin ESP phase (phase 1)') +
  facet_wrap( ~ variation)

# posttest

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist,variation) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug() +
  labs(fill = 'robot\nresponse\ndistribution') +
  xlab('robot response 1 rate') +
  ylab('picked response 1') +
  facet_wrap( ~ variation) 

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist,picked_v1,variation) %>% 
  summarise(mean_rt = mean(rt)) %>% 
  ggplot(aes(reg_dist,mean_rt,fill = picked_v1)) +
  geom_boxplot() +
  theme_bw() +
  geom_rug() +
  facet_wrap( ~ reg_rate + variation)
  
d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,variation) %>% 
  summarise(mean_rt = mean(rt)) %>% 
  ggplot(aes(reg_rate,mean_rt)) +
  geom_boxplot() +
  theme_bw() +
  geom_rug() +
  facet_wrap( ~ variation)

# posttest vs baseline

d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  count(base,baseline_log_odds,reg_dist,picked_v1,variation) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = log((`TRUE`+1)/(`FALSE`+1))) %>% 
  ggplot(aes(baseline_log_odds,post_test_log_odds,colour = reg_dist)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ variation)

d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  count(base,baseline_log_odds,reg_dist,picked_v1,variation) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = log((`TRUE`+1)/(`FALSE`+1))) %>% 
  ggplot(aes(baseline_log_odds,post_test_log_odds,colour = reg_dist)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ variation)

# dude, whatever

library(lme4) 
library(broom.mixed)

# lakok only
# fit2 = glmer(picked_v1 ~ 1 + baseline_log_odds * reg_dist + reg_rate + (1|part_id) + (1|base), data = posttest, family = 'binomial')
# tidy(fit2)
# fit3 = glmer(picked_v1 ~ 1 + baseline_log_odds + reg_dist + reg_rate + (1|part_id) + (1|base), data = posttest, family = 'binomial')
# anova(fit2,fit3)
# lakok/cselekszenek
fit2b = glmer(picked_v1 ~ 1 + baseline_log_odds * reg_dist * variation + reg_rate + (1|part_id) + (1|base), data = posttest, family = 'binomial')
tidy(fit2b)
fit3b = glmer(picked_v1 ~ 1 + baseline_log_odds * variation + reg_dist + reg_rate + (1|part_id) + (1|base), data = posttest, family = 'binomial')
tidy(fit3b)
fit4b = glmer(picked_v1 ~ 1 + baseline_log_odds * reg_dist + variation + reg_rate + (1|part_id) + (1|base), data = posttest, family = 'binomial')
tidy(fit4b)
anova(fit2b,fit3b)
anova(fit2b,fit4b)
