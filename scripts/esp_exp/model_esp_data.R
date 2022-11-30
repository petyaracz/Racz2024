# -- setup -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(bayestestR)

d = read_tsv('exp_data/esp/esp_master_lakok.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

# -- wrangling -- #

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

# -- model -- #

# fit1 = stan_glmer(picked_v1 ~ reg_rate * reg_dist * baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest, cores = 8, chains = 8, iter = 3000)
# 
# fit2 = stan_glmer(picked_v1 ~ reg_rate + reg_dist * baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest, cores = 8, chains = 8, iter = 3000)
# 
# fit3 = stan_glmer(picked_v1 ~ reg_rate + reg_dist + baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest, cores = 8, chains = 8, iter = 3000)
# 
# fit4 = stan_glmer(picked_v1 ~ reg_dist * reg_rate + reg_dist * baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest, cores = 8, chains = 8, iter = 3000)
# 
# fit5 = stan_glmer(picked_v1 ~ reg_rate + baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest, cores = 8, chains = 8, iter = 3000)

# save(fit1, file = 'models/fit1.Rda')
# save(fit2, file = 'models/fit2.Rda')
# save(fit3, file = 'models/fit3.Rda')
# save(fit4, file = 'models/fit4.Rda')
# save(fit5, file = 'models/fit5.Rda')

# loo_fit1 = loo(fit1)
# loo_fit2 = loo(fit2)
# loo_fit3 = loo(fit3)
# loo_fit4 = loo(fit4)
# loo_fit5 = loo(fit5)
# 
# save(loo_fit1, file = 'models/loo_fit1.Rda')
# save(loo_fit2, file = 'models/loo_fit2.Rda')
# save(loo_fit3, file = 'models/loo_fit3.Rda')
# save(loo_fit4, file = 'models/loo_fit4.Rda')
# save(loo_fit5, file = 'models/loo_fit5.Rda')

load('models/fit1.Rda');load('models/fit2.Rda');load('models/fit3.Rda');load('models/fit4.Rda');load('models/fit5.Rda')
load('models/loo_fit1.Rda');load('models/loo_fit2.Rda');load('models/loo_fit3.Rda');load('models/loo_fit4.Rda');load('models/loo_fit5.Rda')

any(rhat(fit1))>1.05
any(rhat(fit2))>1.05
any(rhat(fit3))>1.05
any(rhat(fit4))>1.05
any(rhat(fit5))>1.05

loo_compare(loo_fit1,loo_fit2)
loo_compare(loo_fit2,loo_fit3)
loo_compare(loo_fit1,loo_fit3)
loo_compare(loo_fit2,loo_fit4)
loo_compare(loo_fit1,loo_fit4)
loo_compare(loo_fit2,loo_fit5) # !
loo_compare(loo_fit1,loo_fit5)
loo_compare(loo_fit2,loo_fit6)
loo_compare(loo_fit1,loo_fit6)
loo_compare(loo_fit5,loo_fit6)

# bayesfactor(fit2,fit5)

fit5
fit2

# waic(fit5) # We recommend trying loo instead. 
# waic(fit2) # We recommend trying loo instead. 

# - -viz -- #
mcmc_areas(fit2, pars = vars(reg_ratelow,reg_distreversed,baseline_log_odds,`reg_distreversed:baseline_log_odds`))
brms::hypothesis(fit2, 'reg_distreversed:baseline_log_odds < 0')
mcmc_areas(fit5, pars = vars(reg_ratelow,baseline_log_odds))
# pp_check(fit2, type = 'overlaid')
# mcmc_hist(fit2, pars = vars(reg_ratelow,reg_distreversed,baseline_log_odds,`reg_distreversed:baseline_log_odds`))
# posterior_vs_prior(fit2, pars="beta")
# posterior_interval(fit2) %>% 
#   round(2)
# mcmc_scatter(fit2, pars = c('baseline_log_odds','reg_distreversed'))
# brms::hypothesis()

## larger word-level predictors

fit7 = stan_glmer(picked_v1 ~ reg_rate + reg_dist * derivational + (1|part_id) + (1|base), family = binomial, data = posttest, cores = 8, chains = 8, iter = 3000)

fit7
mcmc_areas(fit7, pars = vars(reg_ratelow,reg_distreversed,`derivational-lik`,`derivational-zik`,`reg_distreversed:derivational-lik`,`reg_distreversed:derivational-zik`))
