#############################################
#############################################
# fit models on main exp: esp/matching game and posttest
#############################################
#############################################

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
# esp / matching game
##########################################

esp = filter(esp, variation != 'hotelban/hotelben')
esp$base = as.factor(esp$base)
esp$part_id = as.factor(esp$part_id)
esp$reg_dist_variation = as.factor(interaction(esp$reg_dist,esp$variation))
esp$reg_rate_variation = as.factor(interaction(esp$reg_rate,esp$variation))
esp$reg_rate_dist = as.factor(interaction(esp$reg_rate,esp$reg_dist))
esp$reg_rate_dist_variation = as.factor(interaction(esp$reg_rate,esp$reg_dist,esp$variation))

# -- syntax -- #

# we don't drop terms, we test interactions

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

fit_e = map(formula_e, ~ bam(formula = as.formula(.), data = esp, family = binomial("logit"), method = 'REML')) # gam needs 'as.formula()'

fits_e = tibble(
  interaction_e,formula_e,fit_e
)

# -- check -- #

map(fits_e$fit_e, ~ gam.check(.))

# looks fine?

itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[2]]) # 1 better
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[3]]) # 1 better
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[4]]) # 4 lower aic than 1 but not much better fit
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[5]]) # 1 better
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[6]]) # 6 better
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[7]]) # 1 better
itsadug::compareML(fits_e$fit_e[[1]],fits_e$fit_e[[8]]) # 8 better
itsadug::compareML(fits_e$fit_e[[6]],fits_e$fit_e[[7]]) # 6 better
itsadug::compareML(fits_e$fit_e[[6]],fits_e$fit_e[[8]]) # 6 better
itsadug::compareML(fits_e$fit_e[[4]],fits_e$fit_e[[6]]) # 6 better
itsadug::compareML(fits_e$fit_e[[5]],fits_e$fit_e[[6]]) # 6 better
itsadug::compareML(fits_e$fit_e[[3]],fits_e$fit_e[[2]]) # 2 > 3 or 2 == 3 in any case pick 2

summary(fits_e$fit_e[[6]])

# this suggests a best model which has dist * variation but only as parametric effects

# let's explore that

fit9 = bam(esp_match ~ reg_rate + reg_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML')
fit10 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML')
fit11 = bam(esp_match ~ reg_rate_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML')
fit12 = bam(esp_match ~ reg_rate_dist_variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'REML')

fit6 = fits_e$fit_e[[6]]

itsadug::compareML(fit6,fit9)
itsadug::compareML(fit6,fit10)
itsadug::compareML(fit6,fit11)
itsadug::compareML(fit6,fit12)
itsadug::compareML(fit9,fit10)
itsadug::compareML(fit9,fit11)
itsadug::compareML(fit10,fit11)
itsadug::compareML(fit10,fit12)
itsadug::compareML(fit9,fit12)
itsadug::compareML(fit10,fit11)

# the best one has main effects but no interaction

# check for random smooths:

fit10fr = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), method = 'fREML', discrete = T)
fit10bfr = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(i) + s(base, bs="re") + s(part_id, bs="re") + s(i, part_id, bs="fs", m=1), data = esp, family = binomial("logit"), method = 'fREML', discrete = T)
# this is overfit.

itsadug::compareML(fit10,fit9)
itsadug::compareML(fit10,fit11)
itsadug::compareML(fit10,fit12)

plot(fit10)

# if this is a linear effect for i and a quadratic effect for baseline log odds I may well fit a glmer.

esp$abs_baseline_log_odds_jitter = abs(esp$baseline_log_odds_jitter)

# ... and check for this particular interaction.

best_fit_e = glmer(esp_match ~ 1 + reg_rate + reg_dist * scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base), data = esp, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)))
best_fit_e_2 = glmer(esp_match ~ 1 + reg_rate + reg_dist + scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base), data = esp, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)))

anova(best_fit_e,best_fit_e_2)
compare_performance(best_fit_e,best_fit_e_2) %>% 
  select(AIC,BIC)

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

plot(compare_performance(fits_p$fit_p, metrics = 'common'))

# based on aic, bic, loss, marginal r2, overfitting info, this seems to boil down to 2 vs 3
fit1 = fits_p$fit_p[[1]]
fit2 = fits_p$fit_p[[2]]
fit3 = fits_p$fit_p[[3]]

plot(compare_performance(fit1,fit2,fit3, metrics = 'common'))

binned_residuals(fit1)
binned_residuals(fit2)
binned_residuals(fit3)

# I'm scared of bayes factors because of data colada. BIC is still useful as an indicator
anova(fit2,fit1)
anova(fit2,fit3)

check_collinearity(fit2)
check_collinearity(fit3)

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

anova(fit2,fit2b)
anova(fit2,fit2c)
binned_residuals(fit2b)
# okay right fit2b seems overfit and doesn't really help with anything.
# why residuals? distribution not a good match? unseen variation?

# anyway.

# best model:

tidy(fit2, conf.int = T) %>%
  filter(effect == 'fixed') %>% 
  select(term,estimate,conf.low,conf.high)

plot(effects::allEffects(fit2))
