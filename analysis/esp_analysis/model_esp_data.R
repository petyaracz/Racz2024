# -- setup -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(rstanarm)
library(lme4)
library(ggthemes)
library(broom.mixed)
library(patchwork)
library(tictoc)

d1 = read_tsv('exp_data/esp/esp_master_lakok.tsv')
d2 = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

# -- wrangling -- #
d1$part_yob = as.double(d1$part_yob)

d = bind_rows(d1,d2)

d = b %>% 
  select(base,log_odds,derivational,nsyl) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  right_join(d)

esp = d %>% 
  filter(trial_kind == 'esp trial') %>% 
  mutate(
    derivational = fct_relevel(derivational, '-szik'),
    two_syl = nsyl == 2,
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )

posttest = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    derivational = fct_relevel(derivational, '-szik'),
    two_syl = nsyl == 2,
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )

# -- viz -- #

posttest_viz_1 = posttest %>% 
  count(baseline_log_odds,variation,reg_dist,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE`+1)))

posttest_viz_2 = posttest %>% 
  count(baseline_log_odds,variation,reg_rate,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE`+1)))

posttest_viz_3 = posttest %>% 
  count(baseline_log_odds,variation,reg_rate,reg_dist,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE`+1)))

posttest_viz_1 %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,colour = reg_dist)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ variation) +
  geom_hline(yintercept = 0, lty = 1) +
  scale_colour_colorblind()

posttest_viz_2 %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,colour = reg_rate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ variation) +
  geom_hline(yintercept = 0, lty = 1) +
  scale_colour_colorblind()

posttest_viz_2 %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,colour = reg_dist)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ variation) +
  geom_hline(yintercept = 0, lty = 1) +
  scale_colour_colorblind()
posttest_viz_3 %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,colour = reg_dist)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ variation + reg_rate) +
  geom_hline(yintercept = 0, lty = 1) +
  scale_colour_colorblind()
  
# -- model -- #

fit1 = glmer(picked_v1 ~ 1 + reg_rate * reg_dist * baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

fit2 = glmer(picked_v1 ~ 1 + reg_rate * reg_dist * baseline_log_odds + baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

fit3 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds + baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

fit3a = glmer(picked_v1 ~ 1 + reg_rate + reg_dist + baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

fit3b = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds + variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

fit4 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

fit5 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist + baseline_log_odds + variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'))

anova(fit3,fit3a)
anova(fit3,fit3b)
anova(fit3,fit4)
anova(fit3,fit5)

exp((BIC(fit3) - BIC(fit3a))/2)
exp((BIC(fit3) - BIC(fit3b))/2)
exp((BIC(fit3) - BIC(fit4))/2)
exp((BIC(fit3) - BIC(fit5))/2)

tidy(fit3)

# anyway.

