# loop this up as everything is called `fit`
setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(rstanarm)
library(broom.mixed)
library(brms)

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



# fit2, fit3: add diagnostic file (like below) and run for waaay more iterations
# fit_1 <- stan_glm(mpg ~ wt + qsec + am, data = mtcars,
#                   chains = 2, cores = 2, iter = 5000,
#                   diagnostic_file = file.path(tempdir(), "df.csv"))
# fit2_bridge = bridgesampling::bridge_sampler(fit2)
# fit3_bridge = bridgesampling::bridge_sampler(fit3)
# bf(fit2_bridge,fit3_bridge)
