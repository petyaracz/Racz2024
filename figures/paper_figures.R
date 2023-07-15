# -- header -- #

set.seed(1337)

setwd('~/Github/Racz2024/')
library(tidyverse)
library(magrittr)
library(glue)
library(broom.mixed)
library(ggthemes)
library(gghalves)
library(patchwork)
library(lme4)

# -- fun -- #

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

logOdds = function(dat,var){
  dat %>% 
    pivot_wider(names_from = {{var}}, values_from = n, values_fill = 0) %>% 
    mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1)))
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

# -- src -- #

source('analysis/esp_analysis/source_esp.R')

# -- figures -- #

# fig 0

b %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  mutate(
    name = ifelse(str_detect(variation, 'lakok'), '1. levelling', '2. vowel deletion')
  ) %>% 
  ggplot(aes(log_odds)) +
  geom_vline(xintercept = 0) +
  geom_histogram(colour = 'darkgrey', fill = 'lightgrey') +
  theme_few() +
  facet_wrap( ~ name) +
  xlab('baseline log odds')

ggsave('figures/fig0.pdf', width = 7, height = 3.5) 
ggsave('figures/fig0.png', width = 7, height = 3.5) 

# fig 1

tidy(esp_fit, conf.int = T) %>% 
  filter(effect == 'fixed') %>% 
  rename(term2 = term) %>% 
  add_column(term = c('Intercept','rate of v1: low','typicality reversed','abs. baseline log odds (scaled)','pattern: vowel deletion', 'trial number (scaled)', 'typicality reversed : abs. baseline log odds (scaled)')) %>% 
  select(term,estimate,std.error,statistic,conf.low,conf.high) %>% 
  mutate(term = term %>% 
           factor(levels = c('Intercept','rate of v1: low','typicality reversed','abs. baseline log odds (scaled)','pattern: vowel deletion', 'trial number (scaled)', 'typicality reversed : abs. baseline log odds (scaled)')) %>% 
           fct_rev()
  ) %>% 
  ggplot(aes(term,estimate)) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_few() +
  coord_flip()

ggsave('figures/fig1.pdf', width = 7, height = 3.5) 
ggsave('figures/fig1.png', width = 7, height = 3.5) 

# fig 2

p1 = esp %>% 
  group_by(i,reg_rate) %>% 
  espIndex +
  facet_wrap( ~ reg_rate, nrow = 1) +
  ylim(.4,.7) +
  xlab('Matching trial number') +
  scale_x_continuous(breaks = c(1,25,54))
p2 = esp %>% 
  group_by(i,reg_dist) %>% 
  espIndex +
  facet_wrap( ~ reg_dist, nrow = 1) +
  ylim(.4,.7) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab('Matching trial number') +
  scale_x_continuous(breaks = c(1,25,54))
p3 = esp %>% 
  group_by(i,var) %>% 
  espIndex +
  facet_wrap( ~ var, nrow = 1) +
  ylim(.4,.7) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab('Matching trial number') +
  scale_x_continuous(breaks = c(1,25,54))

p1 + p3 + p2 + plot_annotation(title = 'Mean trial match with co-player')
ggsave('figures/fig2.pdf', width = 8, height = 4)
ggsave('figures/fig2.png', width = 8, height = 4)

## fig 3

p1 = ranef(esp_fit)$part_id %>% 
  ggplot(aes(`(Intercept)`)) +
  geom_histogram(bins = 42) +
  theme_few() +
  xlab('participant random intercepts')
p2 = ranef(esp_fit)$base %>% 
  ggplot(aes(`(Intercept)`)) +
  geom_histogram(bins = 42) +
  theme_few() +
  xlab('target word random intercepts')

p1 + p2 + plot_annotation(title = 'Random intercepts in the matching game')

ggsave('figures/fig3.pdf', width = 8, height = 4)
ggsave('figures/fig3.png', width = 8, height = 4)

# fig 4

esp %>% 
  group_by(base,variation,reg_dist,abs_baseline_log_odds_jitter) %>% 
  summarise(pred = mean(pred)) %>% 
  ggplot(aes(abs_baseline_log_odds_jitter,pred)) +
  geom_point(alpha = .1) +
  # geom_smooth(method = 'lm', colour = 'white') +
  theme_few() +
  facet_wrap( ~ reg_dist) +
  geom_smooth(method = 'lm', colour = 'lightgray') +
  xlab('baseline preference for variant 1 or 2 (absolute log odds)') +
  ylab('predicted match with co-player') +
  ggtitle('Mean word match with co-player across word absolute baseline log odds\nand regularisation distribution in Matching task')

ggsave('figures/fig4.pdf', width = 8, height = 4)
ggsave('figures/fig4.png', width = 8, height = 4)

# fig 5

tidy(posttest_fit, conf.int = T) %>% 
  filter(effect == 'fixed') %>% 
  rename(term2 = term) %>% 
  add_column(term = c('Intercept','rate of v1: low','typicality reversed','baseline log odds (scaled)','pattern: vowel deletion', 'typicality reversed : abs. baseline log odds (scaled)')) %>% 
  select(term,estimate,std.error,statistic,conf.low,conf.high) %>% 
  mutate(term = term %>% 
           factor(levels = c('Intercept','rate of v1: low','typicality reversed','baseline log odds (scaled)','pattern: vowel deletion', 'typicality reversed : abs. baseline log odds (scaled)')) %>% 
           fct_rev()
  ) %>% 
  ggplot(aes(term,estimate)) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_few() +
  coord_flip()

ggsave('figures/fig5.pdf', width = 8, height = 4)
ggsave('figures/fig5.png', width = 8, height = 4)

# fig 6

posttest %>% 
  group_by(base,variation,reg_dist,baseline_log_odds) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean,colour = reg_dist)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_colorblind() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (predicted)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Mean word variant 1 across word baseline log odds and\nregularisation distribution in posttest')

ggsave('figures/fig6.pdf', width = 8, height = 4)
ggsave('figures/fig6.png', width = 8, height = 4)

# fig 7

posttest %>% 
  group_by(base,variation,reg_dist,baseline_log_odds,var) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean,colour = var)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_viridis_d() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (predicted)') +
  labs(colour = 'morphological pattern') +
  ggtitle('Mean word variant 1 across word baseline log odds and\nregularisation distribution in posttest') +
  facet_wrap( ~ reg_dist)

ggsave('figures/fig7.pdf', width = 8, height = 4)
ggsave('figures/fig7.png', width = 8, height = 4)

# fig 8

tidy(s_esp_fit, conf.int = T) %>% 
  filter(effect == 'fixed') %>% 
  rename(term2 = term) %>% 
  add_column(term = c('Intercept','abs. baseline log odds (scaled)','pattern: levelling', 'trial number (scaled)')) %>% 
  select(term,estimate,std.error,statistic,conf.low,conf.high) %>% 
  mutate(term = term %>% 
           factor(levels = c('Intercept','abs. baseline log odds (scaled)','pattern: levelling', 'trial number (scaled)')) %>% 
           fct_rev()
  ) %>% 
  ggplot(aes(term,estimate)) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_few() +
  coord_flip()

ggsave('figures/fig8.pdf', width = 8, height = 4)
ggsave('figures/fig8.png', width = 8, height = 4)

# fig 9

sesp %>% 
  group_by(i) %>% 
  espIndex() + 
  ggtitle('Mean trial match with co-player, matching part of integration exp') +
  xlab('Matching trial number')

ggsave('figures/fig9.pdf', width = 7, height = 3.5)  
ggsave('figures/fig9.png', width = 7, height = 3.5)  

# fig 10

tidy(s_posttests_fit, conf.int = T) %>% 
  filter(effect == 'fixed') %>% 
  rename(term2 = term) %>% 
  add_column(term = c('Intercept','second post-test','pattern: levelling','baseline log odds (scaled)','second post-test : pattern: levelling')) %>% 
  select(term,estimate,std.error,statistic,conf.low,conf.high) %>% 
  mutate(term = term %>% 
           factor(levels = c('Intercept','second post-test','pattern: levelling','baseline log odds (scaled)','second post-test : pattern: levelling')) %>% 
           fct_rev()
  ) %>% 
  ggplot(aes(term,estimate)) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  theme_few() +
  coord_flip()

ggsave('figures/fig10.pdf', width = 8, height = 4)
ggsave('figures/fig10.png', width = 8, height = 4)

# fig 11

sposttests %>% 
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
  ggtitle('Mean word variant 1 across word baseline log odds and post tests') +
  facet_wrap( ~ var2)

ggsave('figures/fig11.pdf', width = 8, height = 4)
ggsave('figures/fig11.png', width = 8, height = 4)
