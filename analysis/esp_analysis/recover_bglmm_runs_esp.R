library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(bayestestR)
load('models/glm/fit1.Rda')
load('models/glm/fit2.Rda')
load('models/glm/fit3.Rda')
load('models/glm/fit4.Rda')
load('models/glm/fit5.Rda')
load('models/glm/fit6.Rda')
load('models/glm/fit7.Rda')
load('models/glm/fit8.Rda')
load('models/glm/loo_fit1.Rda')
load('models/glm/loo_fit2.Rda')
load('models/glm/loo_fit3.Rda')
load('models/glm/loo_fit4.Rda')
load('models/glm/loo_fit5.Rda')
load('models/glm/loo_fit6.Rda')
load('models/glm/loo_fit7.Rda')

fit1
fit2
fit3
fit4
fit5
fit6
fit7

loo_compare(loo_fit2,loo_fit3)
loo_compare(loo_fit4,loo_fit3)
loo_compare(loo_fit4,loo_fit5)
loo_compare(loo_fit5,loo_fit3)
loo_compare(loo_fit4,loo_fit6)
loo_compare(loo_fit5,loo_fit6)
loo_compare(loo_fit3,loo_fit6)
loo_compare(loo_fit7,loo_fit6)

plot(fit6, 'areas', regex_pars = '^([Irv]|baseli)')
tidy(fit6, conf.int = T)
plot(fit6, 'rhat')
plot(fit6, 'neff')
plot(fit6, "neff_hist")
plot(fit6, "acf")
plot(fit6, "dens_overlay", pars = "(Intercept)")
effective_sample(fit6)
effective_sample(fit7)
effective_sample(fit3)
effective_sample(fit8)
fit6
