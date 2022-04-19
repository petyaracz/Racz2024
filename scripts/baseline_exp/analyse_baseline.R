############################################
# EDA
############################################

# pull in nonce word scores, calculate log odds per prompt word, visualise log odds across main dimensions
# pull in real word log odds, visualise main dimensions
# cross plots to see if they look similar

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(gghalves)
library(ggthemes)
library(patchwork)

set.seed(1337)

# -- functions -- #

transcribe = function(nonce_word,direction){
  
  case_when(
    direction == 'single' ~ nonce_word %>% 
      str_replace_all(., c('cs' = 'ç', 'sz' = 'ß', 'zs' = 'Ω', 'ty' = '†', 'gy' = '©', 'ny' = '¥', 'ly' = '¬')),
    direction == 'double' ~ nonce_word %>% 
      str_replace_all(., c('ç' = 'cs', 'ß' = 'sz', 'Ω' = 'zs', '†' = 'ty', '©' = 'gy', '¥' = 'ny', '¬' = 'ly')),
    T ~ 'wrong direction, either (to) "single" or "double"'
  )
}

BoxViolin = function(d,ez,az){
  ggplot(d, aes({{ez}},log_odds, colour = {{az}})) +
    geom_point(position=position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)) +
    geom_violin(alpha = .5, position = position_dodge(width = 0.6)) +
    geom_boxplot(alpha = .5, width = .15, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, lty = 3) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.1,.25,.5,.75,.9), name = 'p'), name = 'log odds') +
    theme_bw() +
    scale_colour_colorblind()
}

# -- read -- #

r2 = read_tsv('exp_data/baseline/baseline_tidy_sum.tsv')
s = read_tsv('resource/real_words/all_pairs_webcorpus2.tsv')
seg = read_csv('resource/segment_strength.csv')

# -- wrangle -- #

# wrangle source data
s2 = s %>% 
  mutate(
    type = 'real word corpus freq',
    derivational = str_replace(derivational, '^','-')
    ) %>% 
  select(base,base_tr,variation,type,log_odds,derivational,nsyl,vowel,suffix)

# combine
d = bind_rows(r2,s2)

# -- figz -- #

## mindenki

p0 = d %>% 
  filter(type == 'nonce word response') %>%
  ggplot(aes(variation,log_odds)) +
  geom_half_dotplot(dotsize = .3) +
  geom_half_violin() +
  geom_half_boxplot(width = .2) +
  geom_hline(yintercept = 0, lty = 3) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.1,.25,.5,.75,.9), name = 'p'), name = 'log odds') +
  theme_bw()

## lakik

p1 = d %>% 
  filter(
    variation == 'lakom/lakok',
    nsyl %in% 1:2
    ) %>%
  mutate(`n syl` = as.factor(nsyl)) %>% 
  BoxViolin(`n syl`,type) +
  guides(colour = 'none') +
  ggtitle('number of syllables')

p2 = d %>% 
  filter(
    variation == 'lakom/lakok',
    !is.na(derivational)
  ) %>%
  mutate(derivational = fct_relevel(derivational, '-lik','-zik','-szik')) %>% 
  BoxViolin(derivational,type) +
  ggtitle('base ending')
  
## dzsungel

p3 = d %>% 
  filter(
    variation == 'hotelban/hotelben',
    !is.na(suffix)
    ) %>%
  BoxViolin(suffix,type) +
  guides(colour = 'none') +
  ggtitle('suffix type')

p4 = d %>% 
  filter(
    variation == 'hotelban/hotelben'
  ) %>%
  BoxViolin(vowel,type) +
  ggtitle('second vowel')

## cselekszik

p5 = d %>% 
  filter(
    variation == 'cselekszenek/cselekednek',
    !is.na(suffix)
  ) %>% 
  mutate(suffix = fct_relevel(suffix, 'tek','nek','ünk')) %>% 
  BoxViolin(suffix,type) +
  guides(colour = 'none') +
  ggtitle('suffix type')

p6 = d %>% 
  filter(
    variation == 'cselekszenek/cselekednek',
    nsyl %in% 1:2
  ) %>% 
  mutate(`n syl` = as.factor(nsyl)) %>% 
  BoxViolin(`n syl`,type) +
  guides(colour = 'none') +
  ggtitle('number of syllables')

p7 = d %>% 
  filter(
    variation == 'cselekszenek/cselekednek',
    !is.na(derivational)
  ) %>% 
  mutate(derivational = fct_relevel(derivational, '-lik','-zik','-szik')) %>%
  BoxViolin(derivational,type) +
  ggtitle('base ending')

# -- graph plots -- #

p0
ggsave('vis/baseline_0.pdf', width = 8, height = 6)
p1 + p2 + plot_annotation(title = 'lakom/lakok')
ggsave('vis/baseline_1.pdf', width = 12, height = 6)
p3 + p4 + plot_annotation(title = 'hotelban/hotelben')
ggsave('vis/baseline_2.pdf', width = 12, height = 6)
p5 + p6 + p7 + plot_annotation(title = 'cselekszenek/cselekednek')
ggsave('vis/baseline_3.pdf', width = 18, height = 6)
