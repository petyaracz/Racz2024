# -- header -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(glue)
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

interactions = c(
# no interactions
'reg_rate + reg_dist + baseline_log_odds_jitter + variation',
# baseline x dist interaction should definitely show
'reg_rate + reg_dist * baseline_log_odds_jitter + variation',
# it would be splendid if this depended on variation
'reg_rate + baseline_log_odds_jitter * reg_dist * variation',
# I guess reg rate effect could depend on variation too
'reg_rate * variation + baseline_log_odds_jitter * reg_dist * variation',
# This might work but with no interaction w/ variation for distxbasel which is awkward
'reg_rate * variation + baseline_log_odds_jitter * reg_dist',
# This could be a thing
'reg_rate * baseline_log_odds_jitter * reg_dist + variation',
# or we just overfit this
'reg_rate * variation * baseline_log_odds_jitter * reg_dist'
)

formulae = glue('picked_v1 ~ 1 + {interactions} + (1|part_id) + (1|base)')

# -- model fitting -- #

tic('fitting takes this long:')
for (i in 1:7){
  glue('fitting model {i}: {formulae[i]}...')
  fit = stan_glmer(formula = ., data = posttest, family = binomial(link = 'logit'), iter = 4000, chains = 12, cores = 12, refresh = 0)
  glue('saving model {i}...')
  save(fit, file = glue('models/glm/fit{i}.Rda'))
  glue('model {i} saved.')
}
toc()

tic('loo takes this long:')
for (i in 1:7){
  glue('fitting loo {i}: {formulae[i]}...')
  fit = load(glue('models/glm/fit{i}.Rda'))
  glue('saving loo for model {i}...')
  loo_fit = loo(fit, k_threshold = .7, cores = 14)
  save(loo_fit, file = glue('models/loo_fit{i}.Rda'))
  glue('loo for model {i} saved.')
}
toc()