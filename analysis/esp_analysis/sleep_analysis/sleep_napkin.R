library(tidyverse)
library(magrittr)
library(glue)
library(lme4)
library(broom.mixed)
library(ggthemes)
library(gghalves)

setwd('~/Github/Racz2024')

d = read_tsv('exp_data/esp/esp_sleep_pt1_pt2_master_all_filtered.tsv')
e = read_tsv('exp_data/esp/esp_master_all_filtered.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')

# -- header -- #

set.seed(1337)

options(MC.cores=parallel::detectCores())

# -- fun -- #

newIndex = . %>% 
  arrange(trial_index) %>% 
  group_by(dat_id) %>% 
  mutate(i = 1:n()) %>% 
  ungroup()

espIndex = function(dat){
  summarise(dat, mean = mean(pred), se = sd(pred)/sqrt(n()),upper = mean + se, lower = mean - se) %>%
    ggplot() +
    geom_hline(yintercept = .5, lty = 2) +
    geom_line(aes(i,mean)) +
    geom_line(aes(i,upper), lty = 3) +
    geom_line(aes(i,lower), lty = 3) +
    theme_few() +
    xlab('ESP trial number') +
    scale_x_continuous(breaks = c(1,seq(10,50,10),54)) +
    ylab('mean rate of matching co-player (predicted)')
}

cloudPlot2 = function(dat,pred,out){
  dat %>% 
    ggplot(aes({{pred}},{{out}})) +
    geom_hline(yintercept = 0.5) +
    geom_half_violin(side = 'r') +
    geom_half_boxplot(width = .1, side = 'r') +
    geom_half_point(width = .25, side = 'l') +
    theme_bw() +
    coord_flip()
}

# -- wrangle -- #

e %<>% 
  filter(reg_rate == 'low', reg_dist == 'reversed', trial_kind == 'posttest trial') %>% 
  mutate(part_kind = 'bme', part_edu = as.double(part_edu))

d = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  inner_join(d) %>% 
  newIndex()

e = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  inner_join(e) %>% 
  newIndex()

# -- comp w/ esp -- #

d2 = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(part_kind = 'prolific') %>% 
  bind_rows(e)

d2 %>% 
  count(base,baseline_log_odds,picked_v1,trial_kind,part_kind) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = (`TRUE` + 1) / (`FALSE` + 1)) %>%
  ggplot(aes(baseline_log_odds,post_test_log_odds, colour = part_kind)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

fit1 = glmer(picked_v1 ~ 1 + part_kind * scale(baseline_log_odds) + (1|part_id) + (1|base), data = d2, family = binomial)
summary(fit1)
fit2 = glmer(picked_v1 ~ 1 + part_kind + scale(baseline_log_odds) + (1|part_id) + (1|base), data = d2, family = binomial)
summary(fit2)

anova(fit1,fit2)
performance::test_bf(fit1,fit2)

# -- learning in esp -- #

esp = filter(d, trial_kind == 'esp trial') %>% 
  mutate(abs_baseline_log_odds = abs(baseline_log_odds))

esp %>% 
  count(esp_match,i,variation) %>% 
  pivot_wider(names_from = esp_match, values_from = n) %>% 
  mutate(p_match = `TRUE`/(`TRUE`+`FALSE`)) %>% 
  ggplot(aes(i,p_match)) +
  geom_line() +
  theme_few() +
  facet_wrap(~ variation)
  
fit3 = glmer(esp_match ~ 1 + scale(abs_baseline_log_odds) + variation + scale(i) + (1|part_id) + (1|base), 
                data = esp, 
                family = binomial(link = 'logit'), 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
                ))
tidy(fit3, conf.int = T) %>% 
  write_tsv('~/Github/Racz2024/analysis/esp_analysis/sleep_analysis/esp2_fit.tsv')

esp$pred = predict(fit3)

esp %>% 
  group_by(i) %>% 
  espIndex() + 
  ggtitle('Mean trial match with co-player, matching part of integration exp') +
  xlab('Matching trial number')

ggsave('analysis/esp_analysis/preds/pred21.png', width = 8, height = 4)


# -- comp w/ retest -- #

d %>% 
  filter(str_detect(trial_kind, 'posttest trial')) %>% 
  count(base,baseline_log_odds,picked_v1,trial_kind,variation) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = (`TRUE` + 1) / (`FALSE` + 1)) %>%
  ggplot(aes(baseline_log_odds,post_test_log_odds, colour = trial_kind)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap(~ variation)

posttests = d %>% 
  filter(str_detect(trial_kind, 'posttest trial'))

fit4 = glmer(picked_v1 ~ 1 + trial_kind * variation * scale(baseline_log_odds) + (1|part_id) + (1|base), data = posttests, family = binomial)
fit5 = glmer(picked_v1 ~ 1 + trial_kind * scale(baseline_log_odds) + variation + (1|part_id) + (1|base), data = posttests, family = binomial)
fit6 = glmer(picked_v1 ~ 1 + trial_kind + scale(baseline_log_odds) + variation + (1|part_id) + (1|base), data = posttests, family = binomial)
fit7 = glmer(picked_v1 ~ 1 + trial_kind * variation + scale(baseline_log_odds) + (1|part_id) + (1|base), data = posttests, family = binomial)

performance::compare_performance(fit4,fit5,fit6,fit7) %>% 
  select(AIC,BIC,R2_conditional,Log_loss)
performance::test_bf(fit6,fit4)
performance::test_bf(fit6,fit5)
performance::test_bf(fit6,fit7)
anova(fit6,fit7)

# okay, sold
summary(fit7)

tidy(fit7, conf.int = T) %>% 
  write_tsv('~/Github/Racz2024/analysis/esp_analysis/sleep_analysis/posttest2_fit.tsv')

posttests$pred = predict(fit7)

posttests %>% 
  mutate(
    trial_kind = ifelse(trial_kind == 'posttest trial', 'first posttest trial', trial_kind),
    ylab = ifelse(variation == 'lakok/lakom', 'levelling','vowel deletion'),
    ylab = factor(interaction(ylab,trial_kind, sep = '; '),
                  levels = c(
                    'levelling; second posttest trial',
                    'levelling; first posttest trial',
                    'vowel deletion; second posttest trial',
                    'vowel deletion; first posttest trial'
                    )
                  )
         ) %>% 
    group_by(part_id,ylab) %>% 
    summarise(mean = mean(pred)) %>%  
  cloudPlot2(ylab,mean) +
  ggtitle('Mean participant use of variant 1') +
  ylab('Regularisation rate') +
  xlab('Variation; test session')
ggsave('analysis/esp_analysis/preds/pred22.png', width = 6, height = 4)
