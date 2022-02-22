# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)

# -- read -- #

dzsungel = read_tsv('resource/nonce_words/nouns_filt_sample.tsv')
eszek = read_tsv('resource/nonce_words/ik_filt_sample.tsv')
cselekszek = read_tsv('resource/nonce_words/ep_filt_sample.tsv')

# -- filter -- #

dzsungel %>% count(vowel_e,keep_enough_distance,keep_no_overlap,keep_self_diff)
eszek %>% count(nsyl,keep_enough_distance,keep_no_overlap,keep_self_diff)
cselekszek %>% count(nsyl,keep_enough_distance,keep_no_overlap,keep_self_diff)
cselekszek %>% filter(keep_enough_distance,keep_no_overlap,keep_self_diff) %>% count(ending)

# e and é in balance in subset
dzsungel2 = dzsungel %>%
  filter(keep_enough_distance,keep_no_overlap,keep_self_diff)

eszek2 = eszek %>%
  filter(keep_enough_distance,keep_no_overlap,keep_self_diff)

cselekszek2 = cselekszek %>%
  filter(keep_enough_distance,keep_no_overlap,keep_self_diff)

# -- sample -- #

dzsungel3 = dzsungel2 %>% 
  mutate(target = glue('{target_front} / {target_back}')) %>% 
  select(prompt,suffix_back,target) %>% 
  pivot_wider(id_cols = prompt, names_from = suffix_back, values_from = target) %>% 
  sample_n(500)

eszek2l = eszek2 %>% 
  mutate(target = glue('{target_k} / {target_m}')) %>% 
  select(prompt,nsyl,target) %>% 
  group_split(nsyl)

eszek3 = bind_rows(
  sample_n(eszek2l[[1]],200),
  sample_n(eszek2l[[2]],300)
)

cselekszek_keep = cselekszek2 %>% 
  distinct(prompt_cc,prompt_cvc) %>% 
  group_by(prompt_cc) %>% 
  sample_n(1) %>% 
  ungroup()

cselekszek2 = cselekszek2 %>% 
  inner_join(cselekszek_keep)
  
cselekszek2l = cselekszek2 %>% 
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
  select(nsyl,prompt,suffix_ep,target) %>% 
  pivot_wider(id_cols = c(nsyl,prompt), names_from = suffix_ep, values_from = target) %>% 
  group_split(nsyl)

cselekszek3 = bind_rows(
  sample_n(cselekszek2l[[1]],150),
  sample_n(cselekszek2l[[2]],350)
)

# -- write -- #

dzsungel3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

eszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

cselekszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')
