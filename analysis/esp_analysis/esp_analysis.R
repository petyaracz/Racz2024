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

esp = filter(esp, variation != 'hotelban/hotelben')
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

map(fits_e$fit_e, ~ gam.check(.))

# f1 and particularly f2 seems underfit, f4 dodgy, f8 super overfit probably

itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[2]]) # 2 lower aic than 1, not better fit
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[3]]) # 3 lower aic than 1, not better fit
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[4]]) 
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[5]])
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[6]]) # 6 lower aic than 1, better fit
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[7]])
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[8]])
itsadug::compareML(fits_e$fit_e[[2]],fits_e$fit_e[[6]]) # 2 lower aic than 6, not better fit
itsadug::compareML(fits_e$fit_e[[3]],fits_e$fit_e[[6]]) # 3 lower aic than 6, better fit
itsadug::compareML(fits_e$fit_e[[4]],fits_e$fit_e[[6]]) # 4 > 6
itsadug::compareML(fits_e$fit_e[[5]],fits_e$fit_e[[6]]) # 5 > 6
itsadug::compareML(fits_e$fit_e[[3]],fits_e$fit_e[[2]]) # 2 > 3 or 2 == 3 in any case pick 2

plot(fits_e$fit_e[[1]])
plot(fits_e$fit_e[[2]])

summary(fits_e$fit_e[[2]])

# this suggests a best model which has dist * variation but only as parametric effects

fit9 = bam(esp_match ~ reg_rate + reg_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)
fit10 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)
fit11 = bam(esp_match ~ reg_rate_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)
fit12 = bam(esp_match ~ reg_rate_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML', nthreads = 16)

itsadug::compareML(fit10,fit9)
itsadug::compareML(fit10,fit11)
itsadug::compareML(fit10,fit12)

summary(fit10)
# Formula:
#   esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + 
#   s(i) + s(base, bs = "re") + s(part_id, bs = "re")
# 
# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        0.68593    0.06256  10.964   <2e-16 ***
#   reg_ratelow                       -0.08149    0.06063  -1.344    0.179    
# reg_distreversed                  -0.52480    0.06066  -8.651   <2e-16 ***
#   variationcselekszenek/cselekednek -0.09722    0.06458  -1.506    0.132    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf  Ref.df Chi.sq  p-value    
# s(baseline_log_odds_jitter)  6.876   7.918 104.26  < 2e-16 ***
#   s(i)                         1.002   1.004  11.14 0.000854 ***
#   s(base)                     22.012 310.000  23.73 0.158376    
# s(part_id)                  72.544 164.000 130.59  < 2e-16 ***

plot(fit10)
# if this is a linear effect for i I may well fit a glmer.

esp$abs_baseline_log_odds_jitter = abs(esp$baseline_log_odds_jitter)

best_fit_e = glmer(esp_match ~ 1 + reg_rate + reg_dist * scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base), data = esp, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)))
best_fit_e_2 = glmer(esp_match ~ 1 + reg_rate + reg_dist + scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base), data = esp, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)))

anova(best_fit_e,best_fit_e_2)
compare_performance(best_fit_e,best_fit_e_2)

# best fit:

tidy(best_fit_e, conf.int = T)

##########################################
# posttest
##########################################

posttest = filter(posttest, variation != 'hotelban/hotelben')

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

formula_p = glue('picked_v1 ~ 1 + {interaction_p} + (1|part_id) + (1|base)')

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
fit1 = fits_p$fit_p[[1]]
fit2 = fits_p$fit_p[[2]]
fit3 = fits_p$fit_p[[3]]

binned_residuals(fit1)
binned_residuals(fit2)
binned_residuals(fit3)

test_bf(fit1,fit2)
test_bf(fit2,fit3)
anova(fit2,fit1)
anova(fit2,fit3)

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

test_bf(fit2,fit2b)
anova(fit2,fit2b)
binned_residuals(fit2b)

# okay right fit2b seems overfit and doesn't really help with anything.
# why residuals? distribution not a good match? unseen variation?

# anyway.

# best model:

tidy(fit2, conf.int = T) %>%
  filter(effect == 'fixed') %>% 
  select(term,estimate,conf.low,conf.high)

plot(effects::allEffects(fit2))
