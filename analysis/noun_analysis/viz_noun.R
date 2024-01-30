#############################################
#############################################
# noun model: results
#############################################
#############################################

# -- header -- #

setwd('~/Github/Racz2024/')
library(tidyverse)
library(glue)
library(magrittr)
library(patchwork)
library(ggthemes)
library(lme4)
# library(mgcv)
library(performance)
library(sjPlot)
# library(broom.mixed)
# library(parallel)

# -- source -- #

source('analysis/noun_analysis/source_noun.R')

# -- viz -- #

# baseline: strong é == back preference
b %>% 
  mutate(
    base = fct_reorder(base, log_odds),
    vowel = fct_relevel(vowel, 'é')
    ) %>%
  select(base,vowel,resp1,resp2) %>% 
  pivot_longer(resp1:resp2, names_to = 'resp', values_to = 'n') %>%
  ggplot(aes(y = base, x = n, fill = resp)) +
  geom_col() +
  # legend labels: back/front
  scale_fill_manual(values = c('grey','black'), labels = c('back','front')) +
  # no y axis label
  ylab(NULL) +
  facet_wrap( ~ vowel)

# baseline: no difference across suffixes (nice)
b %>% 
  ggplot(aes(log_odds,suffix,colour = vowel)) +
  geom_boxplot() +
  theme_bw()

# esp: reversed is harder to learn but learning happens

esp %>% 
  count(reg_dist,reg_rate,esp_match,trial_index) %>% 
  pivot_wider(names_from = esp_match, values_from = n) %>% 
  mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1))) %>% 
  ggplot(aes(x = trial_index, y = log_odds, color = reg_dist, lty = reg_rate)) +
  geom_smooth() +
  theme_bw() +
  scale_color_colorblind()
  
# posttest

# ... 
  
fit1 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + (1 + 1|part_id) + (1|base), 
                     data = posttest, 
                     family = binomial(link = 'logit'), 
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
                     ))
fit2 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist + baseline_log_odds_jitter + (1 + 1|part_id) + (1|base), 
             data = posttest, 
             family = binomial(link = 'logit'), 
             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
             ))


plot(compare_performance(fit1, fit2, metrics = 'common'))
test_likelihoodratio(fit1,fit2)
broom.mixed::tidy(fit1, conf.int = T)
# pincer pezsgot