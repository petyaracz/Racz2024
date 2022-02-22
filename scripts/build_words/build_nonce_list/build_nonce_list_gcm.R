####################################
# GCM
# @raczpetya
####################################

# gcm needs training set with two (here at least) categories and transcribed forms
# and target set with transcribed forms

# training sets are:
# either
# for nouns: bisyllabic front and back nouns, with a freq cutoff
# for ik verbs: verbs ending in ik and not ending in ik
# for ep verbs: ik verbs, stable ccik and cvcik
# or
# forms themselves.
# test sets are: 
# nonce forms.

# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

nc = read_tsv('resource/real_words/noun_bag.tsv')
vc = read_tsv('resource/real_words/verb_bag.tsv')

rfh = read_tsv('resource/real_words/front_harmony/fh_pairs_webcorpus2.tsv') 
rik = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
rep = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- create training sets -- #

## nouns
n_real = rfh %>% 
  buildGCMtraining() %>% 
  rename('word' = tr) %>% 
  select(word,log_odds)

n_training = nc %>% 
  filter(vh %in% c('front','back'), lemma_freq > 100) %>% 
  mutate(
    word = transcribe(lemma, 'single'),
    category = vh
    ) %>% 
  select(word,category)

## ik verbs
ik_real = rik %>% 
  buildGCMtraining() %>% 
  mutate(word = str_replace(tr, '.$', '')) %>% 
  select(word,log_odds)

vc %<>% 
  mutate(
    ik = str_detect(lemma, 'ik$'),
    word = str_replace(lemma, 'ik$', '') %>% 
      transcribe('single')
    )
ik_training = vc %>% 
  mutate(category = ifelse(ik, 'ik','no_ik')) %>% 
  select(word,category)

## ep verbs
# we build a stem thing for the gcm to target
rep_matcher = rep %>%
  filter(xpostag == '[/V][Inf]') %>% 
  mutate(
    form_1tr = transcribe(form_1,'single'),
    stem_tr = transcribe(stem,'single')
         ) %>% 
  mutate(
    verb = str_replace(form_1tr, glue('(?<={stem_tr}.).*$'), 'ik')
  ) %>% 
  select(verb,stem)

# we add that to the dataset and transform the whole thing
ep_real = rep %>% 
  left_join(rep_matcher) %>% 
  select(-stem) %>% 
  rename('stem' = verb) %>% 
  buildGCMtraining() %>% 
  filter(!is.na(tr)) %>% 
  mutate(word = str_replace_all(tr, c('melegdik' = 'melegßik', 'çelekdik' = 'çelekßik', '©arapdik' = '©arapßik', 'verekdik' = 'verekßik'))) %>%  # whoops
  select(word,log_odds)

ep_training = vc %>% 
  filter(
    ik,
    !(word %in% ep_real$word)
         ) %>% 
  mutate(
    word = transcribe(lemma, 'single'),
    category = case_when(
      str_detect(word, '[^aáeéiíoóöőuúüű][aáeéiíoóöőuúüű][^aáeéiíoóöőuúüű](?=ik)') ~ 'cvc',
      str_detect(word, '[^aáeéiíoóöőuúüű]{2}(?=ik)') ~ 'cc'
      )    
    ) %>% 
  select(word,category)

# -- gcm test run -- #

n_gcm_real = furGCM(n_training,n_real, var_s = .3, var_p = 1, distance_metric = 'lv')

left_join(n_gcm_real,n_real) %>%
  ungroup() %>% 
  summarise(cor = cor(back,log_odds))

ep_gcm_real = furGCM(ep_training,ep_real, var_s = .3, var_p = 1, distance_metric = 'lv')

left_join(ep_gcm_real,ep_real) %>%
  ungroup() %>% 
  summarise(cor = cor(cc,log_odds))

ik_gcm_real = furGCM(ik_training,ik_real, var_s = .3, var_p = 1, distance_metric = 'lv')

left_join(ik_gcm_real,ik_real) %>%
  ungroup() %>% 
  summarise(cor = cor(ik,log_odds))
