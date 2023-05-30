# -- header -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(glue)
library(magrittr)
library(patchwork)
library(lme4)
library(mgcv)
library(performance)
library(broom.mixed)
library(parallel)

# -- source -- #

source('analysis/esp_analysis/source_esp.R')

##########################################
# esp
##########################################

esp$base = as.factor(esp$base)
esp$part_id = as.factor(esp$part_id)
esp$reg_dist_variation = as.factor(interaction(esp$reg_dist,esp$variation))
esp$reg_rate_variation = as.factor(interaction(esp$reg_rate,esp$variation))
esp$reg_rate_dist = as.factor(interaction(esp$reg_rate,esp$reg_dist))
esp$reg_rate_dist_variation = as.factor(interaction(esp$reg_rate,esp$reg_dist,esp$variation))

# -- syntax -- #

interaction_e = c(
  # no interactions
  'reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i)',
  # variation x index
  'reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = variation)',
  # dist x index
  'reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = reg_dist)',
  # rate x index
  'reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = reg_rate)',
  # rate x dist x index
  'reg_rate_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = reg_rate_dist)',
  # variation x dist x index
  'reg_rate + reg_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = reg_dist_variation)',
  # rate x variation x index
  'reg_dist + reg_rate_variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = reg_rate_variation)',
  # rate x dist x index
  'reg_rate_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(i, by = reg_rate_dist_variation)'
)

formula_e = glue('esp_match ~ {interaction_e} + s(base, bs="re") + s(part_id, bs="re")')

# -- fit -- #

fit_e = map(formula_e, ~ bam(formula = as.formula(.), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)) # gam needs 'as.formula()'

# save(fit_e, file = 'fits_e.Rda')

fits_e = tibble(
  interaction_e,formula_e,fit_e
)

# -- check -- #

# for (i in 1:nrow(fits_e)){
#   plot(check_model(fits_e$fit_e[[i]])) + plot_annotation(title = fits_e$interaction[[i]])
#   ggsave(glue('analysis/esp_analysis/diagnostics/diagnostic_e{i}.pdf'), width = 8, height = 10)
# }

map(fits_e$fit_e, ~ gam.check(.))

# 8 overfit, 4 badly fit?

itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[2]])
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[3]])
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[4]]) # 3 better
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[5]])
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[6]]) # 6 better
itsadug::compareML(fits_e$fit_e[[3]],fits_e$fit_e[[6]]) # sameish
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[7]]) # sameish
itsadug::compareML(fits_e$fit_e[[3]],fits_e$fit_e[[7]])
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[8]])

plot(fits_e$fit_e[[1]])
plot(fits_e$fit_e[[3]])

summary(fits_e$fit_e[[1]])
summary(fits_e$fit_e[[3]])
summary(fits_e$fit_e[[6]])

# this suggests a best model which has dist * variation but only as parametric effects

fit9 = bam(esp_match ~ reg_rate + reg_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)
fit10 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)
fit11 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(i, part_id, bs = "fs") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16) # run dis

itsadug::compareML(fit9,fit10)

summary(fit10)
# Formula:
#   esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + 
#   s(i) + s(base, bs = "re") + s(part_id, bs = "re")
# 
# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        0.66465    0.06185  10.747   <2e-16 ***
#   reg_ratelow                       -0.03776    0.05994  -0.630   0.5287    
# reg_distreversed                  -0.54667    0.05996  -9.118   <2e-16 ***
#   variationcselekszenek/cselekednek -0.10775    0.06371  -1.691   0.0908 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf  Ref.df Chi.sq  p-value    
# s(baseline_log_odds_jitter)  6.960   7.966  97.72  < 2e-16 ***
#   s(i)                         1.004   1.008  12.65 0.000385 ***
#   s(base)                     27.841 321.000  30.59 0.103851    
# s(part_id)                  80.570 166.000 158.89  < 2e-16 ***

plot(fit10)
# if this is a linear effect for i I may well fit a glmer.

best_fit_e = glmer(esp_match ~ 1 + reg_rate + reg_dist + variation + baseline_log_odds_jitter + i + (1|part_id) + (1|base), data = posttest, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)))

##########################################
# posttest
##########################################

# -- syntax -- #

interaction_p = c(
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
  'reg_rate * variation * baseline_log_odds_jitter * reg_dist',
  # this might be a thing
  'reg_rate * variation * reg_dist + baseline_log_odds_jitter',
  # also this
  'reg_rate * reg_dist + variation + baseline_log_odds_jitter'
)

formula_p = glue('picked_v1 ~ 1 + {interaction} + (1|part_id) + (1|base)')

# -- fit -- #

fit_p = map(formula_p, ~ glmer(formula = as.formula(.), data = posttest, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000))))

fits_p = tibble(
  interaction_p,formula_p,fit_p
)

# -- check -- #

for (i in 1:9){
  plot(check_model(fits_p$fit_p[[i]])) + plot_annotation(title = fits_p$interaction_p[[i]])
  ggsave(glue('analysis/esp_analysis/diagnostics/diagnostic_p{i}.pdf'), width = 8, height = 10)
}

# more complex models look overfit, which should be no surprise to anyone
# 7 and 8 are out for sure
# binned residuals are slightly worrying, so once we pick the best models, we should think about random effects
perf_p = compare_performance(fits_p$fit_p)
perf_p %>% 
  select(Name,AIC,BIC,R2_conditional,R2_marginal,RMSE,Log_loss)

# based on aic, bic, loss, marginal r2, overfitting info, this seems to boil down to 2 vs 3
fit2 = fits$fit[[2]]
fit3 = fits$fit[[3]]

binned_residuals(fit2)
binned_residuals(fit3)
# some of this might be the bad participants that we will rerun anyway

test_bf(fit1,fit2)
test_bf(fit2,fit3)
anova(fit2,fit3)
anova(fit2,fit1)

# this is pretty clearly going for fit2

fit2b = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1 + baseline_log_odds_jitter|part_id) + (1|base), 
              data = posttest, 
              family = binomial(link = 'logit'), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
              ))
fit2c = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1|part_id) + (1 + reg_dist |base), 
              data = posttest, 
              family = binomial(link = 'logit'), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
              ))
# fit2c is overfit right out of the box
binned_residuals(fit2c) # this is in fact worse
fit3b = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter * variation + (1 + baseline_log_odds_jitter|part_id) + (1|base), 
              data = posttest, 
              family = binomial(link = 'logit'), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
              ))
fit3c = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter * variation + (1|part_id) + (1 + reg_dist |base), 
              data = posttest, 
              family = binomial(link = 'logit'), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
              ))
# same for fit3c
binned_residuals(fit3c)

# sum: BIC supports the simpler model, goodness-of-fit and AIC support the more complex model.

# eh

# cv
posttest %<>% 
  sample_n(n()) 
training = posttest[1:7500,]  
test = posttest[7501:9180,]  

fit2t = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1 + baseline_log_odds_jitter|part_id) + (1|base), 
              data = training, 
              family = binomial(link = 'logit'), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
              ))
fit3t = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter * variation + (1 + baseline_log_odds_jitter|part_id) + (1|base), 
              data = training, 
              family = binomial(link = 'logit'), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
              ))
cor(test$picked_v1,predict(fit2t, test, type = 'response'))
cor(test$picked_v1,predict(fit3t, test, type = 'response'))

# jaj

tidy(fit2, conf.int = T) %>%
  filter(effect == 'fixed') %>% 
  select(term,estimate,conf.low,conf.high)

tidy(fit3, conf.int = T) %>%
  filter(effect == 'fixed') %>% 
  select(term,estimate,conf.low,conf.high)

## summary

# there are three meaningful possibilities
# (1) no interaction. esp fits, posttest aic, bic, loss say otherwise
# (2) there is baseline x dist x variation interaction. esp fits, posttest aic, loss support this
# (3) there is only baseline x dist interaction, lakok and cselekszenek are the same. posttest bf / bic says this. also cv and probably bayesian GLM, but I'm not super sure about what that does exactly

# based on aic, loss, esp: variation x dist effect.
# based on bic: 