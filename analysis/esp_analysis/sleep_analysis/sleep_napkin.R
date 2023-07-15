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

logOdds = function(dat,var){
  dat %>% 
    pivot_wider(names_from = {{var}}, values_from = n, values_fill = 0) %>% 
    mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1))) %>% 
    select(-`FALSE`,-`TRUE`)
} 

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
    # geom_hline(yintercept = 0.5) +
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

write_tsv(d, '~/Github/Racz2024/exp_data/sleep/sleep.tsv')

e = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  inner_join(e) %>% 
  newIndex()

fit7 = glmer(picked_v1 ~ 1 + trial_kind * variation + scale(baseline_log_odds) + (1|part_id) + (1|base), data = posttests, family = binomial)

posttests = d %>% 
  filter(str_detect(trial_kind, 'posttest trial'))

posttests$pred = predict(fit7)

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
AIC(fit1,fit2)
BIC(fit1,fit2)

# eh

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

write_tsv(esp, '~/Github/Racz2024/exp_data/sleep/sleep_esp.tsv')

esp %>% 
  group_by(i) %>% 
  espIndex() + 
  ggtitle('Mean trial match with co-player, matching part of integration exp') +
  xlab('Matching trial number')

ggsave('analysis/esp_analysis/preds/pred21.png', width = 8, height = 4)
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Duesseldorf_talk/pic10.pdf', width = 8, height = 4)

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

# posttests = d %>%
#   filter(str_detect(trial_kind, 'posttest trial'))

# fit4 = glmer(picked_v1 ~ 1 + trial_kind * variation * scale(baseline_log_odds) + (1|part_id) + (1|base), data = posttests, family = binomial)
# fit5 = glmer(picked_v1 ~ 1 + trial_kind * scale(baseline_log_odds) + variation + (1|part_id) + (1|base), data = posttests, family = binomial)
# fit6 = glmer(picked_v1 ~ 1 + trial_kind + scale(baseline_log_odds) + variation + (1|part_id) + (1|base), data = posttests, family = binomial)
# fit7 = glmer(picked_v1 ~ 1 + trial_kind * variation + scale(baseline_log_odds) + (1|part_id) + (1|base), data = posttests, family = binomial)
# fit8 = glmer(picked_v1 ~ 1 + trial_kind + scale(baseline_log_odds) * variation + (1|part_id) + (1|base), data = posttests, family = binomial)

# performance::compare_performance(fit4,fit5,fit6,fit7,fit8) %>% 
  # select(AIC,BIC,R2_conditional,Log_loss)
# anova(fit6,fit7)
# anova(fit7,fit8)

# tidy(fit6, conf.int=T)

# okay, sold
tidy(fit7, conf.int=T)

tidy(fit7, conf.int = T) %>% 
  write_tsv('~/Github/Racz2024/analysis/esp_analysis/sleep_analysis/posttest2_fit.tsv')

posttests$pred = predict(fit7)

posttests %<>% 
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
         )

posttests_logodds = posttests %>% 
  count(ylab,base,picked_v1,variation) %>% 
  logOdds(picked_v1) %>% 
  mutate(ylab = as.character(ylab))

posttests_logodds = b %>% 
  filter(base %in% posttests_logodds$base) %>% 
  distinct(base,log_odds,variation) %>% 
  mutate(ylab = glue('{variation}; baseline')) %>% 
  # rename(log_odds = baseline_log_odds) %>% 
  bind_rows(posttests_logodds) %>% 
  mutate(type = str_extract(ylab, '(baseline|first|second)'))

posttests_logodds %>% 
  mutate(
    type = factor(type, levels = c('second','first','baseline')),
    var2 = ifelse(variation == 'lakok/lakom', '1. levelling', '2. vowel deletion')
         ) %>% 
  cloudPlot2(type,log_odds) +
  facet_wrap( ~ var2) +
  ylab('raw log odds') +
  xlab('test') +
  ggtitle('Raw log odds across baseline - test - retest in integration exp')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Duesseldorf_talk/pic11.pdf', width = 8, height = 4)  
posttests %>% 
  mutate(
    pt = ifelse(trial_kind == 'posttest trial', 'immediate', 'after sleep'),
    pt = factor(pt, levels = c('immediate', 'after sleep')),
    var2 = ifelse(variation == 'lakok/lakom', '1. levelling', '2. vowel deletion')
         ) %>% 
  group_by(base,baseline_log_odds,var2,pt) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean, colour = pt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_brewer(palette = 'Accent') +
  ylab('preference for variant 1 (predicted)') +
  xlab('baseline preference for variant 1 (log odds)') +
  labs(colour = 'post test') +
  ggtitle('Mean word variant 1 across word baseline log odds and post test in the integration task') +
  facet_wrap( ~ var2)
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Duesseldorf_talk/pic12.pdf', width = 8, height = 4)
  

lo1 = posttests %>% 
  filter(str_detect(trial_kind, 'post')) %>% 
  count(base,variation,trial_kind,picked_v1) %>% 
  logOdds('picked_v1') %>% 
  select(base,variation,trial_kind,log_odds)

lo2 = posttests %>% 
  distinct(base,variation,baseline_log_odds) %>% 
  rename(log_odds = baseline_log_odds) %>% 
  mutate(trial_kind = 'baseline')

bind_rows(lo1,lo2) %>% 
  mutate(trial_kind = factor(trial_kind, levels = c('second posttest trial', 'first posttest trial', 'baseline'))) %>% 
  ggplot() +
  geom_violin(aes(trial_kind, log_odds)) +
  geom_boxplot(width = .1, aes(trial_kind, log_odds)) +
  geom_line(aes(trial_kind, log_odds, group = base), alpha = .2) +
  theme_few() +
  facet_wrap( ~ variation, ncol = 1) +
  xlab('test session') +
  ylab('log odds of variant 1 / variant 2') +
  coord_flip()
ggsave('analysis/esp_analysis/preds/pred23.png', width = 6, height = 4)
