# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)

dzsungel = read_tsv('resource/nonce_words/nouns_final.tsv')
eszek = read_tsv('resource/nonce_words/ik_final.tsv')
cselekszek = read_tsv('resource/nonce_words/ep_final.tsv')

# -- nouns -- #

dzsungel2 = dzsungel %>% 
  mutate(target = glue('{target_front} / {target_back}')) %>% 
  select(prompt,suffix_back,target) %>% 
  pivot_wider(id_cols = prompt, names_from = suffix_back, values_from = target) %>% 
  sample_n(500)
  
eszek2 = eszek %>% 
  mutate(target = glue('{target_k} / {target_m}')) %>% 
  select(prompt,target) %>% 
  sample_n(500)

cselekszek2 = cselekszek %>% 
  mutate(target = glue('{target_cc} / {target_cvc}')) %>% 
  select(prompt_cc,prompt_cvc,suffix_ep,target) %>% 
  pivot_wider(id_cols = c(prompt_cc,prompt_cvc), names_from = suffix_ep, values_from = target) %>% 
  sample_n(500)


# -- write -- #

dzsungel2 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

eszek2 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

cselekszek2 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')
