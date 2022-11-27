setwd('~/Github/Racz2024/')
library(tidyverse)

d = read_tsv('exp_data/esp/esp_master.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

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

# okay a model won't hurt
library(lme4)
library(broom.mixed)
posttest = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )
posttest %>% 
  distinct(part_id,reg_rate,reg_dist) %>% 
  count(reg_rate,reg_dist)
fit1 = glmer(picked_v1 ~ reg_rate * reg_dist + (1|part_id) + (1|base), family = binomial, data = posttest)
summary(fit1)
plot(effects::allEffects(fit1))
fit2 = glmer(picked_v1 ~ reg_rate + reg_dist + (1|part_id) + (1|base), family = binomial, data = posttest)
summary(fit2)

# comparisons

compo = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds)

compo = d %>% 
  count(reg_rate,reg_dist,base,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n) %>% 
  mutate(esp_log_odds = log((`TRUE` + 1)/(`FALSE` + 1))) %>% 
  select(-`FALSE`,-`TRUE`) %>% 
  left_join(compo) %>% 
  mutate(reg_dist = fct_relevel(reg_dist, 'typical'))

compo %>% 
  mutate(reg_dist = fct_relevel(reg_dist, 'typical')) %>% 
  ggplot(aes(baseline_log_odds,esp_log_odds, colour = reg_dist)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_rate)

fit3 = lmer(esp_log_odds ~ baseline_log_odds * reg_rate * reg_dist+ (1|base), data = compo)
tidy(fit3, conf.int = T)
# plot(effects::allEffects(fit3))
fit4 = lmer(esp_log_odds ~ baseline_log_odds * reg_rate + baseline_log_odds * reg_dist+ (1|base), data = compo)
tidy(fit4, conf.int = T)
plot(effects::allEffects(fit4))
