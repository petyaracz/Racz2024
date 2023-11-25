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
library(faux)

# -- fun -- #

toTidy = . %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  mutate(name = ifelse(str_detect(variation, 'lakok'), '1. levelling', '2. vowel deletion'))

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

b2 = read_tsv('exp_data/baseline/baseline_tidy.tsv')

# -- figures -- #

######
# poster
######

my_density = list(
  # geom_vline(xintercept = 0, lty = 3),
  guides(fill = 'none'),
  theme_void(),
  ylim(0,1),
  xlim(-3,3),
  theme(plot.title = element_text(hjust = 0.5))
)

# design 1

tibble(a = rt(100000, df = 100)) %>% 
  ggplot(aes(a)) +
  geom_density() +
  my_density +
  theme(plot.background = element_rect(fill = "transparent", color = NA))
ggsave('~/Downloads/proba.png', bg = 'transparent', width = 4, height = 4)

# design 2

rn = rnorm_multi(n = 100, 
            mu = c(0, 0, 0),
            sd = c(1, 1, 1),
            r = c(.5,-.5,-.5), 
            varnames = c('a','b','c'),
            empirical = FALSE)

rn %>% 
  ggplot() +
  geom_point(aes(a,b), colour = 'grey', alpha = .5) +
  geom_smooth(aes(a,b), method = 'lm', se = F, colour = 'grey') +
  geom_point(aes(a,c), colour = 'gold', alpha = .5) +
  geom_smooth(aes(a,c), method = 'lm', se = F, colour = 'gold') +
  theme_few() +
  xlab('use of variant 1 in baseline (log odds)') +
  ylab('use of variant 1 by co-player (log odds)') +
  scale_x_continuous(position = 'top')

ggsave('~/Downloads/proba2.pdf', width = 8, height = 4)

# big figure

## learning

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

p_a = p1 + p3 + p2 + plot_annotation(title = 'Learning trajectories in matching game')

## rate

rate1 = posttest %>% 
  count(picked_v1,reg_rate,base) %>% 
  logOdds(picked_v1) %>% 
  select(base,reg_rate,log_odds)

# rate1 = b %>% 
#   filter(base %in% rate1$base) %>% 
#   mutate(reg_rate = 'baseline') %>% 
#   select(base,reg_rate,log_odds) %>% 
#   bind_rows(rate1)

p_b = rate1 %>% 
  ggplot(aes(log_odds, colour = reg_rate)) +
  geom_density() +
  theme_few() +
  xlab('use of variant 1 (log odds)') +
  labs(colour = 'co-player rate') +
  scale_colour_colorblind() +
  ggtitle('Player rates of use in immediate post-test')

# whatever

p_c = posttest %>% 
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
  ggtitle('Player lexical distributions in baseline and immediate post-tests')

p_d = posttest %>% 
  group_by(base,variation,baseline_log_odds,var) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean,colour = var)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_viridis_d() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (predicted)') +
  labs(colour = 'morphological pattern') +
  ggtitle('Player lexical distributions in baseline and immediate post-tests')

p_e = sposttests %>% 
  mutate(
    pt = ifelse(trial_kind == 'posttest trial', 'immediate', 'after sleep'),
    pt = factor(pt, levels = c('immediate', 'after sleep')),
    var2 = ifelse(variation == 'lakok/lakom', '1. levelling', '2. vowel deletion')
  ) %>% 
  group_by(base,baseline_log_odds,pt) %>% 
  summarise(mean = mean(pred)) %>% 
  ggplot(aes(baseline_log_odds,mean, colour = pt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_few() +
  scale_colour_brewer(palette = 'Accent') +
  ylab('preference for variant 1 (predicted)') +
  xlab('baseline preference for variant 1 (log odds)') +
  labs(colour = 'post test') +
  ggtitle('Player lexical distributions in baseline and immediate and delayed post-tests')

p_a / p_b / p_c / p_d / p_e
ggsave('figures/poster/poster_figure.pdf', width = 9, height = 18)
