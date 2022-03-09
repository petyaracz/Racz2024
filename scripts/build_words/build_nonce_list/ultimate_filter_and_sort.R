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

dzsungel %>% count(vowel_e,keep_enough_distance,keep_no_overlap)
eszek %>% count(nsyl,ending,keep_enough_distance,keep_no_overlap)
cselekszek %>% count(nsyl,tr_end,keep_enough_distance,keep_no_overlap)
cselekszek %>% filter(keep_enough_distance,keep_no_overlap) %>% count(nsyl,tr_end)

dzsungel2 = dzsungel %>%
  filter(keep_enough_distance,keep_no_overlap)

eszek2 = eszek %>%
  filter(keep_enough_distance,keep_no_overlap)

cselekszek2 = cselekszek %>%
  filter(keep_enough_distance,keep_no_overlap)

# -- sample -- #

dzsungel3 = dzsungel2 %>% 
  mutate(
    target = glue('{target_front} / {target_back}'),
    vowel = ifelse(vowel_e, 'e', 'é')
    ) %>% 
  select(prompt,suffix_back,target,vowel) %>% 
  pivot_wider(id_cols = c(prompt,vowel), names_from = suffix_back, values_from = target) %>% 
  sample_n(n())

eszek3 = eszek2 %>% 
  mutate(
    ending = str_extract(tr, '.ik$'),
    target = glue('{target_k} / {target_m}'),
    suffix = glue('-{str_replace(ending, "ß", "sz")}')
    ) %>% 
  select(nsyl,suffix,prompt,target) %>% 
  sample_n(n()) %>% 
  arrange(nsyl,suffix)

# count(eszek3,nsyl,suffix)

cselekszek3 = cselekszek2 %>% 
  mutate(
    prompt = glue('{prompt_cc} / {prompt_cvc}'),
    target = glue('{target_cc} / {target_cvc}'),
    suffix = glue('-{str_replace(tr_end, "ß", "sz")}')
    ) %>% 
  select(nsyl,prompt,suffix,suffix_ep,target) %>% 
  pivot_wider(id_cols = c(nsyl,suffix,prompt), names_from = suffix_ep, values_from = target) %>%
  arrange(nsyl,suffix)

# count(cselekszek3,nsyl,suffix)
  
# -- write -- #
"
dzsungel3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

eszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

cselekszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')
"

# -- trial tsv -- #

sample1 = cselekszek %>% 
  filter(keep_enough_distance,keep_no_overlap) %>% 
  sample_n(3) %>% 
  rename(
    prompt = prompt_cc,
    target1 = target_cc,
    target2 = target_cvc,
    category = suffix_ep,
    syllables = nsyl
  ) %>% 
  mutate(
    type = 'ep',
    target_sentence = case_when(
      category == 'na' ~ 'Ő holnap is boldogan...',
      category == 'nak' ~ 'A barátai is sokat...',
      category == 'ni' ~ 'Én is szeretnék...',
      category == 'tak' ~ 'Régen a barátai sokat...',
      category == 'tok' ~ 'Ti is sokat...'
    ),
    carrier_sentence = glue('János sokat {prompt}.')
  ) %>% 
  select(prompt,carrier_sentence,target_sentence,target1,target2,category,syllables) 

sample2 = dzsungel %>% 
  filter(keep_enough_distance,keep_no_overlap) %>% 
  sample_n(3) %>% 
  rename(
    # prompt = prompt_cc,
    target1 = target_front,
    target2 = target_back,
    category = suffix_back
    # syllables = nsyl
  ) %>% 
  mutate(
    type = 'noun',
    target_sentence = case_when(
      category == 'ba' ~ 'Bemegyünk a...',
      category == 'ban' ~ 'Benne vagyunk a...',
      category == 'nak' ~ 'Odaadom a csomagot a...',
      category == 'nál' ~ 'Itt vagyunk a...',
      category == 'ra' ~ 'Ráteszem a dobozt a...'
    ),
    carrier_sentence = glue('Ez itt egy {prompt}.')
  )%>% 
  select(prompt,carrier_sentence,target_sentence,target1,target2,category) 

sample3 = eszek %>% 
  filter(keep_enough_distance,keep_no_overlap) %>% 
  sample_n(3) %>% 
  rename(
    # prompt = prompt_cc,
    target1 = target_k,
    target2 = target_m,
    # category = suffix_back
    syllables = nsyl
  ) %>% 
  mutate(
    type = 'ik',
    carrier_sentence = glue('János sokat {prompt}.'),
    target_sentence = 'Én is sokat...'
  ) %>% 
  select(prompt,carrier_sentence,target_sentence,target1,target2,syllables) 

# sample = bind_rows(sample1,sample2,sample3) %>% 
  # mutate_all( ~replace(., is.na(.), ''))

write_csv(sample1, 'exp/baseline/sample1.csv')
write_csv(sample2, 'exp/baseline/sample2.csv')
write_csv(sample3, 'exp/baseline/sample3.csv')

master = tibble(
  condsFile = c('sample1.csv','sample2.csv','sample3.csv')
)

write_csv(master, 'exp/baseline/master.csv')
