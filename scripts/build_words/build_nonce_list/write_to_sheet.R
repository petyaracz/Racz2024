# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)

dzsungel = read_tsv('resource/nonce_words/nouns_filt_sample.tsv')
eszek = read_tsv('resource/nonce_words/ik_filt_sample.tsv')
cselekszek = read_tsv('resource/nonce_words/ep_filt_sample.tsv')

# -- filter -- #

dzsungel %>% count(vowel_e,keep_enough_distance,keep_no_overlap,keep_self_diff)
eszek %>% count(nsyl,keep_enough_distance,keep_no_overlap,keep_self_diff)
cselekszek %>% count(nsyl,keep_enough_distance,keep_no_overlap,keep_self_diff)

# e and é in balance in subset
dzsungel2 = dzsungel %>%
  filter(keep_enough_distance,keep_no_overlap,keep_self_diff) %>%
  sample_n(n())

eszek1 = eszek %>%
  filter(nsyl==1,keep_enough_distance,keep_no_overlap) %>% 
  sample_n(n())

eszek2 = eszek %>%
  filter(nsyl==2,keep_enough_distance,keep_no_overlap,keep_self_diff) %>%
  sample_n(n())

eszek2 = bind_rows(eszek1,eszek2)

cselekszek1 = cselekszek %>%
  filter(nsyl==1,keep_enough_distance,keep_no_overlap) %>%
  sample_n(n())

cselekszek2 = cselekszek %>%
  filter(nsyl==2,keep_enough_distance,keep_no_overlap,keep_self_diff) %>%
  sample_n(n())

cselekszek2 = bind_rows(cselekszek1,cselekszek2)

# -- nouns -- #

# ok lol
dzsungel3 = dzsungel2 %>% 
  mutate(target = glue('{target_front} / {target_back}')) %>% 
  select(prompt,suffix_back,target) %>% 
  pivot_wider(id_cols = prompt, names_from = suffix_back, values_from = target) %>% 
  sample_n(500)
  # count(str_detect(prompt, 'é')) F 469  T 264

eszek3 = eszek2 %>% 
  mutate(target = glue('{target_k} / {target_m}')) %>% 
  select(prompt,target) %>% 
  sample_n(500)
  # count(str_count(prompt, '[aáeéiíoóüőuúüű]') == 2) T 454 F 422
  
cselekszek3 = cselekszek2 %>% 
  mutate(
    prompt = ifelse(
      str_detect(prompt_cc, 'lik$'),
      prompt_cc,
      glue('{prompt_cc} / {prompt_cvc}')
      ),
    target = ifelse(
      str_detect(suffix_ep, '^n(a[k]?|i)$') & str_detect(prompt_cc, 'szik$'),
      target_cvc,
      glue('{target_cc} / {target_cvc}')
      )
    ) %>% 
  select(prompt,suffix_ep,target) %>% 
  pivot_wider(id_cols = prompt, names_from = suffix_ep, values_from = target) %>% 
  sample_n(500)
# ignore warning, it sorts itself out

# -- write -- #

dzsungel3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

eszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

cselekszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')
