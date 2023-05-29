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

# -- source -- #

source('analysis/esp_analysis/source_esp.R')

##########################################
# esp
##########################################

esp$base = as.factor(esp$base)
esp$part_id = as.factor(esp$part_id)

bam1 = bam(esp_match ~ reg_rate + reg_dist + variation + s(trial_index) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

bam2 = bam(esp_match ~ reg_rate + reg_dist + variation + s(trial_index) + s(base, bs="re") + s(trial_index, part_id, bs="fs", m=1), data = esp, family = binomial("logit"), discrete = T)
# overfit

bam3 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter) + s(trial_index) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

summary(bam3)

bam4 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter, by = variation) + s(trial_index) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

summary(bam4)

itsadug::compareML(bam3,bam4)

bam5 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter, by = reg_dist) + s(trial_index) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

itsadug::compareML(bam3,bam5)

bam6 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter, by = reg_rate) + s(trial_index) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

itsadug::compareML(bam3,bam6)

bam7 = bam(esp_match ~ reg_rate + reg_dist + variation + s(baseline_log_odds_jitter, by = reg_rate) + s(baseline_log_odds_jitter, by = reg_dist) + s(baseline_log_odds_jitter, by = variation) + s(trial_index) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

plot(bam7)
# reg rate int looks similar

bam7 = bam(esp_match ~ reg_rate + reg_dist + variation + s(trial_index, by = reg_rate) + s(trial_index, by = reg_dist) + s(trial_index, by = variation) + s(baseline_log_odds_jitter) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

plot(bam7)

esp$interaction = interaction(esp$variation,esp$reg_dist)

bam8 = bam(esp_match ~ interaction + variation + s(trial_index, by = interaction) + s(baseline_log_odds_jitter) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

plot(bam8)

bam9 = bam(esp_match ~ reg_rate + reg_dist + variation + s(trial_index, by = reg_rate) + s(trial_index, by = reg_dist) + s(baseline_log_odds_jitter) + s(base, bs="re") + s(part_id, bs="re"), data = esp, family = binomial("logit"), discrete = T)

itsadug::compareML(bam8,bam9)

# I've seen enough. there's a variation x reg rate effect.


##########################################
# posttest
##########################################

# -- syntax -- #

interaction = c(
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

formula = glue('picked_v1 ~ 1 + {interaction} + (1|part_id) + (1|base)')

# -- fit -- #

fit = map(formula, ~ glmer(formula = ., data = posttest, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000))))

fits = tibble(
  interaction,formula,fit
)

# -- check -- #

for (i in 1:9){
  plot(check_model(fits$fit[[i]])) + plot_annotation(title = fits$interaction[[i]])
  ggsave(glue('analysis/esp_analysis/diagnostics/diagnostic{i}.pdf'), width = 8, height = 10)
}

# more complex models look overfit, which should be no surprise to anyone
# 7 and 8 are out for sure
# binned residuals are slightly worrying, so once we pick the best models, we should think about random effects
perf = compare_performance(fits$fit)
perf %>% 
  write_tsv('analysis/esp_analysis/diagnostics/performances.tsv')
perf %>% 
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