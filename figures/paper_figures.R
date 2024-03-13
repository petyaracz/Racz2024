# 2024-02-27 revised for resubmission: now with raw data!

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
library(ggpubr)
library(sjPlot)

# -- fun -- #

toTidy = . %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  mutate(name = ifelse(str_detect(variation, 'lakok'), '1. levelling', '2. vowel deletion'))

espPlotRaw = function(dat,var_name){
  dat %>% 
    group_by({{var_name}},i) %>%  # hacker
    summarise(mean = mean(esp_match)) %>% 
    ggplot(aes(i,mean, colour = {{var_name}})) +
    geom_point(alpha = .5) +
    geom_smooth() +
    theme_bw() +
    scale_fill_colorblind() +
    scale_colour_colorblind() + 
    ylim(.35,.8) +
    guides(colour = 'none')
}

logOdds = function(dat,var){
  dat %>% 
    pivot_wider(names_from = {{var}}, values_from = n, values_fill = 0) %>% 
    mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1)))
} 

cloudPlot2 = function(dat,var,out){
  dat %>% 
    ggplot(aes({{var}},{{out}})) +
    geom_hline(yintercept = 0.5) +
    geom_half_violin(side = 'r') +
    geom_half_boxplot(width = .1, side = 'r') +
    geom_half_point(width = .25, side = 'l') +
    theme_bw() +
    coord_flip()
}

# -- src -- #

source('analysis/esp_analysis/source_esp.R')

b2 = read_tsv('exp_data/baseline/baseline_tidy.tsv')

# -- figures -- #

# fig 0

b2 = esp %>% 
  mutate(file_name_2 = str_extract(file_name, '^(cselekszenek|lakok)_[123]')) %>% 
  distinct(base,file_name_2) %>% 
  left_join(b)

b3 = b2 %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  mutate(
    name = ifelse(str_detect(variation, 'lakok'), '1. leveling', '2. vowel deletion'),
    variant_1 = resp1,
    variant_2 = resp2,
    base2 = fct_reorder(base, log_odds)
  ) %>% 
  select(base2,name,variant_1,variant_2,log_odds,file_name_2) %>% 
  pivot_longer(-c(base2,name,log_odds,file_name_2), names_to = 'variant', values_to = 'n') %>% 
  mutate(variant = fct_relevel(variant, 'variant_2'))
  
p1 = b3 %>% 
  filter(str_detect(name, 'leveling')) %>% 
  ggplot(aes(y = base2, x = n, fill = variant)) +
  geom_col() +
  guides(fill = 'none') +
  xlab('Variant 1 / variant 2\nresponses per word') +
  theme_few() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  scale_fill_colorblind()
  
p2 = b3 %>% 
  filter(str_detect(name, 'deletion')) %>% 
  ggplot(aes(y = base2, x = n, fill = variant)) +
  geom_col() +
  guides(fill = 'none') +
  xlab('variant 1 / variant 2\nresponses per word') +
  theme_few() +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  scale_fill_colorblind()

p3 = b3 %>%
  ggplot(aes(log_odds)) +
  geom_histogram() +
  facet_wrap( ~ name, nrow = 2) +
  theme_few() +
  coord_flip() +
  scale_y_reverse() +
  xlab('log ( n variant 1 / n variant 2 )') +
  ylab('form count')
  
(p3 | (p1 / p2 + plot_layout(guides = 'auto'))) + plot_layout(guides = 'collect')

ggsave('figures/fig0.pdf', width = 5, height = 5) 
ggsave('figures/fig0.png', width = 5, height = 5) 

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

ggsave('figures/fig1.pdf', width = 7, height = 2) 
ggsave('figures/fig1.png', width = 7, height = 2) 

# fig 2

p1 = espPlotRaw(esp, reg_rate) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ylab('Trial mean of matching co-player') +
  scale_x_continuous(breaks = c(1, 25, 54), name = '') +
  ggtitle('High / low co-player', subtitle = 'Raw data')

p2 = espPlotRaw(esp, variation) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  scale_x_continuous(breaks = c(1, 25, 54), name = '') +
  ggtitle('Leveling / vowel deletion')

p3 = espPlotRaw(esp, reg_dist) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) +
  scale_x_continuous(breaks = c(1, 25, 54), name = '') +
  ggtitle('Typical / reversed co-player')

p4 = plot_model(esp_fit, 'pred', terms = c('i [all]', 'reg_rate')) +
  theme_bw() +
  ylim(.35,.8) +
  scale_fill_colorblind() +
  scale_colour_colorblind() +
  scale_x_continuous(breaks = c(1, 25, 54), name = '') +
  theme(legend.position="bottom") +
  ylab('Predicted matching co-player') +
  ggtitle('', subtitle = 'Predicted data')

p5 = plot_model(esp_fit, 'pred', terms = c('i [all]', 'variation')) +
  theme_bw() +
  ylim(.35,.8) +
  scale_fill_colorblind(labels = c('leveling','v deletion')) +
  scale_colour_colorblind(labels = c('leveling','v deletion')) +
  scale_x_continuous(breaks = c(1, 25, 54), name = 'Matching trial number') +
  theme(legend.position="bottom") +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  ggtitle('')

p6 = plot_model(esp_fit, 'pred', terms = c('i [all]', 'reg_dist')) +
  theme_bw() +
  ylim(.35,.8) +
  scale_fill_colorblind() +
  scale_colour_colorblind() +
  scale_x_continuous(breaks = c(1, 25, 54), name = '') +
  theme(legend.position="bottom") +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  ggtitle('')

(p1 + p2 + p3) / (p4 + p5 + p6)
ggsave('figures/fig2.pdf', width = 8, height = 7)
ggsave('figures/fig2.png', width = 8, height = 7)

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

ggsave('figures/fig3.pdf', width = 6, height = 3)
ggsave('figures/fig3.png', width = 6, height = 3)

# fig 4

esp %>% 
  mutate(abs_baseline_log_odds = abs(baseline_log_odds)) %>% 
  count(base,variation,reg_dist,abs_baseline_log_odds,esp_match) %>% 
  logOdds(esp_match) %>% 
  ggplot(aes(abs_baseline_log_odds,log_odds)) +
  geom_point(alpha = .1) +
  # geom_smooth(method = 'lm', colour = 'white') +
  theme_few() +
  facet_wrap( ~ reg_dist) +
  geom_smooth(method = 'lm') +
  xlab('baseline preference for variant 1 or 2 (absolute log odds)') +
  ylab('match with co-player (log odds)') +
  ggtitle('Word match with co-player across word absolute\nbaseline log odds and regularisation distribution in Matching task')

ggsave('figures/fig4.pdf', width = 6.5, height = 3.5)
ggsave('figures/fig4.png', width = 6.5, height = 3.5)

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

ggsave('figures/fig5.pdf', width = 7, height = 2)
ggsave('figures/fig5.png', width = 7, height = 2)

# fig 6

posttest %>% 
  count(base,variation,reg_dist,baseline_log_odds,picked_v1) %>% 
  logOdds(picked_v1) %>% 
  ggplot(aes(baseline_log_odds,log_odds,colour = reg_dist)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_colorblind() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post-test preference for variant 1 (log odds)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Word use of variant 1 in post-test vs baseline\nacross regularisation distribution in post-test')

ggsave('figures/fig6.pdf', width = 6.5, height = 3.5)
ggsave('figures/fig6.png', width = 6.5, height = 3.5)

# fig 7

posttest %>% 
  count(base,var,reg_dist,baseline_log_odds,picked_v1) %>% 
  logOdds(picked_v1) %>% 
  ggplot(aes(baseline_log_odds,log_odds,colour = var)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_viridis_d() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post-test preference for\nvariant 1 (log odds)') +
  labs(colour = 'morphological pattern') +
  ggtitle('Word use of variant 1 in post-test vs baseline\nacross regularisation distribution in posttest') +
  facet_wrap( ~ reg_dist)

ggsave('figures/fig7.pdf', width = 6.5, height = 3.5)
ggsave('figures/fig7.png', width = 6.5, height = 3.5)

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

ggsave('figures/fig8.pdf', width = 7, height = 2)
ggsave('figures/fig8.png', width = 7, height = 2)

# fig 9

sesp %>% 
  group_by(i) %>% 
  summarise(mean = mean(esp_match)) %>% 
  ggplot(aes(i,mean)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(breaks = c(1,25,54)) +
  scale_fill_colorblind() +
  scale_colour_colorblind() + 
  ylab('Mean matching co-player') +
  ylim(.35,.8) +
  ggtitle('Mean trial match with co-player,\nmatching part of integration exp') +
  xlab('Matching trial number')

ggsave('figures/fig9.pdf', width = 5, height = 3.5)  
ggsave('figures/fig9.png', width = 5, height = 3.5)  

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

ggsave('figures/fig10.pdf', width = 7, height = 2)
ggsave('figures/fig10.png', width = 7, height = 2)

# fig 11

sposttests %>% 
  mutate(
    pt = ifelse(trial_kind == 'posttest trial', 'immediate', 'after sleep'),
    pt = factor(pt, levels = c('immediate', 'after sleep')),
    var2 = ifelse(variation == 'lakok/lakom', '1. levelling', '2. vowel deletion')
  ) %>% 
  count(base,baseline_log_odds,var2,pt,picked_v1) %>% 
  logOdds(picked_v1) %>% 
  ggplot(aes(baseline_log_odds,log_odds, colour = pt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_brewer(palette = 'Accent') +
  ylab('preference for variant 1 (mean)') +
  xlab('baseline preference for variant 1 (log odds)') +
  labs(colour = 'post test') +
  ggtitle('Word use of variant 1 in post-tests vs baseline') +
  facet_wrap( ~ var2)

ggsave('figures/fig11.pdf', width = 6.5, height = 3.5)
ggsave('figures/fig11.png', width = 6.5, height = 3.5)
