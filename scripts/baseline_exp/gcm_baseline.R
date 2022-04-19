####################################
# GCM
####################################

# todo
# put things in functions
# do eszek and cselekedik

# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)
library(furrr)
library(broom)

plan(multisession, workers = 8)

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

nc = read_tsv('resource/real_words/noun_bag.tsv')
s = read_tsv('resource/real_words/all_pairs_webcorpus2.tsv')
r2 = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')

# -- nouns -- #

## real vs all

training = nc %>% 
  filter(vh %in% c('front','back'), lemma_freq > 100) %>% 
  mutate(
    word = transcribe(lemma,'single'),
    category = vh
         ) %>% 
  select(word,category)

target = s %>% 
  filter(variation == 'hotelban/hotelben') %>%
  rename(word = base_tr) %>% 
  group_by(word,stem,lemma_freq_corrected) %>% 
  summarise(
    freq_1 = sum(freq_1),
    freq_2 = sum(freq_2),
    log_odds = log( ( freq_1 + 1 ) / ( freq_2 + 1 ) )
  ) %>% 
  filter(freq_1 > 10, freq_2 > 10)

n_gcm_real = furGCM(training,target, var_s = .5, var_p = 1, distance_metric = 'lv')

result = inner_join(target,n_gcm_real)

fit1 = glm(cbind(freq_1,freq_2) ~ 1 + back, family = binomial, data = result)
tidy(fit1)
ggplot(result, aes(back,log_odds)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw()

## nonce vs real

training = s %>% 
  filter(variation == 'hotelban/hotelben') %>%
  rename(word = base_tr) %>% 
  group_by(word,stem,lemma_freq_corrected) %>% 
  summarise(
    freq_1 = sum(freq_1),
    freq_2 = sum(freq_2),
    log_odds = log( ( freq_1 + 1 ) / ( freq_2 + 1 ) ),
    category = case_when(
      log_odds >= 1 ~ 'back',
      log_odds <= 1 ~ 'front'
    )
  ) %>% 
  filter(freq_1 > 10, freq_2 > 10, !is.na(log_odds)) %>% 
  ungroup() %>% 
  select(word,category)

target = r2 %>% 
  filter(variation == 'hotelban/hotelben') %>%
  mutate(word = transcribe(base, 'single')) %>% 
  select(word,base,log_odds)

n_gcm_real = furGCM(training,target, var_s = .5, var_p = 1, distance_metric = 'lv')

result = inner_join(target,n_gcm_real)

ggplot(result, aes(back,log_odds)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw()
