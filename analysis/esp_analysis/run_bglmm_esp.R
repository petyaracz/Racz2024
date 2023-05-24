# -- header -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(rstanarm)
library(tictoc)

options(MC.cores=parallel::detectCores())

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

posttest = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    derivational = fct_relevel(derivational, '-szik'),
    two_syl = nsyl == 2,
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )

# -- jitter log odds -- #

posttest$baseline_log_odds_jitter = jitter(posttest$baseline_log_odds, factor = .0001)

# -- model syntax -- #



tic('fitting takes this long:')
fit1 = stan_glmer(picked_v1 ~ 1 + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit1, file = 'models/glm/fit1.Rda')

fit2 = stan_glmer(picked_v1 ~ 1 + reg_rate * reg_dist * baseline_log_odds_jitter * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit2, file = 'models/glm/fit2.Rda')

fit3 = stan_glmer(picked_v1 ~ 1 + reg_rate * + baseline_log_odds + reg_dist * baseline_log_odds_jitter + baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit3, file = 'models/glm/fit3.Rda')

fit4 = stan_glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit4, file = 'models/glm/fit4.Rda')

fit5 = stan_glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + baseline_log_odds * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit5, file = 'models/glm/fit5.Rda')

fit6 = stan_glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8) 
save(fit6, file = 'models/glm/fit6.Rda')

fit7 = stan_glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter * variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit7, file = 'models/glm/fit7.Rda')

fit8 = stan_glmer(picked_v1 ~ 1 + reg_rate + reg_dist + baseline_log_odds_jitter + variation + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 8, cores = 8)
save(fit8, file = 'models/glm/fit8.Rda')
toc()

prior_summary(fit1)

tic('loo takes this long:')
loo_fit1 = loo(fit1, k_threshold = .7, cores = 8)
save(loo_fit1, file = 'models/loo_fit1.Rda')
loo_fit2 = loo(fit2, k_threshold = .7, cores = 8)
save(loo_fit2, file = 'models/loo_fit2.Rda')
loo_fit3 = loo(fit3, k_threshold = .7, cores = 8)
save(loo_fit3, file = 'models/loo_fit3.Rda')
loo_fit4 = loo(fit4, k_threshold = .7, cores = 8)
save(loo_fit4, file = 'models/loo_fit4.Rda')
loo_fit5 = loo(fit5, k_threshold = .7, cores = 8)
save(loo_fit5, file = 'models/loo_fit5.Rda')
loo_fit6 = loo(fit6, k_threshold = .7, cores = 8)
save(loo_fit6, file = 'models/loo_fit6.Rda')
loo_fit7 = loo(fit7, k_threshold = .7, cores = 8)
save(loo_fit7, file = 'models/loo_fit7.Rda')
loo_fit8 = loo(fit8, k_threshold = .7, cores = 8)
toc()