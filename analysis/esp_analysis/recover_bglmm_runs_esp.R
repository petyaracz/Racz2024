# loop this up as everything is called `fit`
setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(rstanarm)
library(broom.mixed)
library(bayestestR)

# load fits
load('models/glm/fit1.Rda')
fit1 = fit
load('models/glm/fit2.Rda')
fit2 = fit
load('models/glm/fit3.Rda')
fit3 = fit
load('models/glm/fit4.Rda')
fit4 = fit
load('models/glm/fit5.Rda')
fit5 = fit
load('models/glm/fit6.Rda')
fit6 = fit
load('models/glm/fit7.Rda')
fit7 = fit
load('models/glm/fit8.Rda')
fit8 = fit
load('models/glm/fit9.Rda')
fit9 = fit

# load loos
load('models/glm/loo_fit1.Rda')
loo_fit1 = loo_fit
load('models/glm/loo_fit2.Rda')
loo_fit2 = loo_fit
load('models/glm/loo_fit3.Rda')
loo_fit3 = loo_fit
load('models/glm/loo_fit4.Rda')
loo_fit4 = loo_fit
load('models/glm/loo_fit5.Rda')
loo_fit5 = loo_fit
load('models/glm/loo_fit6.Rda')
loo_fit6 = loo_fit
load('models/glm/loo_fit7.Rda')
loo_fit7 = loo_fit
load('models/glm/loo_fit8.Rda')
loo_fit8 = loo_fit
load('models/glm/loo_fit9.Rda')
loo_fit9 = loo_fit 

# fits
fits = c(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9)
# ess
map(fits, ~ any(effective_sample(.)$ESS < 1000)) # mindegy jok
# rhat
map(fits, ~ summary(.)[, "Rhat"];rhat[rhat > 1.1]) # mindegy kok
# formuale
map(fits, ~ formula(.)) # mindegy faszom
formula(fit1)
formula(fit2)
formula(fit3)
tidy(fit2,conf.int=T)
tidy(fit3,conf.int=T)
loo_compare(loo_fit2,loo_fit3)
formula(fit4)
formula(fit5)
formula(fit6)
formula(fit9)
fit6
loo_compare(loo_fit2,loo_fit6)
fit7
fit8
fit9
### so this wins:
fit2
# but maybe
fit3
plot(fit2, 'areas', regex = '^(reg|base|vari)')
plot(fit3, 'areas', regex = '^(reg|base|vari)')
plot(fit6, 'areas', regex = '^(reg|base|vari)')
plot(fit9, 'areas', regex = '^(reg|base|vari)')
plot(fit2, "dens_overlay", pars = "(Intercept)")
# needs reg_dist * variation * reg_rate
tidy(fit2, conf.int=T)
loo_fit2
loo_fit3
loo_compare(loo_fit2,loo_fit3)

# whatever here's a plot

d1 = read_tsv('exp_data/esp/esp_master_lakok.tsv')
d2 = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

library(gghalves)
library(ggthemes)

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

posttest %>% 
  count(baseline_log_odds,reg_dist,variation,base,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1))) %>% 
  ggplot(aes(baseline_log_odds,log_odds, colour = reg_dist)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap(~ variation)

library(lme4)

ffit1 = glmer(picked_v1 ~ 1 + variation * reg_dist * baseline_log_odds + reg_rate + (1|part_id) + (1|base), family = binomial, data = posttest)
ffit2 = glmer(picked_v1 ~ 1 + variation + reg_dist * baseline_log_odds + reg_rate + (1|part_id) + (1|base), family = binomial, data = posttest)
anova(ffit1,ffit2)
tidy(ffit1,conf.int=T) %>% View
# no sorry its probs fit2
fit2
# maybe test var instead of mean