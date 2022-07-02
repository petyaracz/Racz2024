setwd('~/Github/Racz2024')

library(tidyverse)
library(magrittr)
library(glue)

library(patchwork)
library(broom)

c = read_tsv('resource/real_words/front_harmony/fh_pairs_webcorpus2.tsv')
hun = read_tsv('resource/hu_list.txt')


c %<>%
  mutate(
    ban_third = ntile(log_odds, 3),
    category = case_when(
        ban_third == 1 ~ 'front',
        ban_third == 3 ~ 'back'
      ),
    h = stem %in% hun$word,
    stem_vowel = str_extract(stem, '[aáoóöőuúüű]'),
    second_vowel = str_extract(stem, '[eé]'),
    final_consonant = str_extract(stem, '.$')
  )

thirds = c %>% 
  filter(h,ban_third %in% c(1,3)) %>% 
  distinct(stem,ban_third) %>% 
  arrange(stem) %>% 
  group_split(ban_third) %>% 
  map(~ pull(., stem))

thirds

c %>% 
  filter(h,ban_third %in% c(1,3)) %>% 
  ggplot(aes(stem_vowel,fill = category)) +
  geom_bar(position = 'dodge') +
  theme_bw()

p1 = c %>% 
  filter(h,ban_third %in% c(1,3)) %>% 
  ggplot(aes(stem_vowel,fill = category)) +
  geom_bar(position = 'dodge') +
  theme_bw() +
  guides(fill = F)

p2 = c %>% 
  filter(h,ban_third %in% c(1,3)) %>% 
  ggplot(aes(second_vowel,fill = category)) +
  geom_bar(position = 'dodge') +
  theme_bw() +
  guides(fill = F)

p3 = c %>% 
  filter(h,ban_third %in% c(1,3)) %>% 
  ggplot(aes(final_consonant,fill = category)) +
  geom_bar(position = 'dodge') +
  theme_bw()

p1 + p2 + p3

c %>% 
  filter(ban_third %in% c(1,3)) %>% 
  ggplot(aes(stem_vowel,fill = category)) +
  geom_bar(position = 'dodge') +
  theme_bw()

ch = filter(c,h)

fit1 = glm(cbind(freq_1,freq_2) ~ 1 + stem_vowel, family = binomial(link = 'logit'), data = ch)
tidy(fit1, conf.int = T)
