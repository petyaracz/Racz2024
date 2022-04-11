# load filtered samples, write them into a google sheet, filter them by hand, load them back in here, split them across input lists, write into exp dir

# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)

"
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

dzsungel3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

eszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

cselekszek3 %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')
"

# -- one filtering later -- #

dzsungel =
  googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

eszek = 
  googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

cselekszek = 
  googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')

# -- filtering -- #

list_length = 162

dzsungel2 = dzsungel %>% 
  filter(is.na(Petya),is.na(Ági)) %>% 
  group_by(vowel) %>% 
  sample_n(list_length / 2) %>% 
  ungroup() %>% 
  select(prompt,vowel,ban,nak,nál) %>% 
  sample_n(n()) %>% 
  arrange(vowel) %>% 
  mutate(keep = rep(c('ban','nak','nál'),list_length/3)) %>% 
  pivot_longer(-c(prompt,vowel,keep), names_to = 'suffix') %>% 
  filter(keep == suffix)

dzsungel3 = dzsungel2 %>% 
  mutate(
    word = prompt,
    target1 = str_extract(value, '^.*(?= /)'),
    target2 = str_extract(value, '(?<=/ ).*$'),
    category = 'dzsungel',
    target_sentence = case_when(
      suffix == 'ban' ~ 'Ott vagyunk a...',
      suffix == 'nak' ~ 'Elneveztem a kutyámat...',
      suffix == 'nál' ~ 'Nincs is jobb egy jó...'
    ),
    carrier_sentence = glue('Ez itt egy {prompt}.')
  )%>% 
  select(word,prompt,suffix,vowel,carrier_sentence,target_sentence,target1,target2,category) %>%
  sample_n(n()) %>% 
  rownames_to_column()

count(dzsungel3, vowel, suffix)

dzsungel3

eszek2 = eszek %>% 
  filter(is.na(Petya),is.na(Ági)) %>% 
  group_by(nsyl) %>% 
  sample_n(list_length / 2) %>% 
  ungroup() %>% 
  select(prompt,nsyl,suffix,target) %>% 
  rename(
    word = prompt,
    derivational = suffix
    )

eszek3 = eszek2 %>% 
  mutate(
    target1 = str_extract(target, '^.*(?= /)'),
    target2 = str_extract(target, '(?<=/ ).*$'),
    v = str_extract(target1, '.(?=k$)'),
    v2 = case_when(
      str_detect(v, '[ao]') ~ 'a',
      str_detect(v, '[eö]') ~ 'e'
                   ),
    prompt = ifelse(derivational == '-lik',
                    str_replace(word, 'ik$', glue('{v2}sz')),
                    str_replace(word, 'ik$', glue('{v}l'))
      ),
    category = 'lakik',
    target_sentence = 'Én is sokat...',
    carrier_sentence = glue('Te bizony sokat {prompt}.')
  ) %>% 
  select(word,prompt,derivational,nsyl,carrier_sentence,target_sentence,target1,target2,category) %>% 
  sample_n(n()) %>% 
  rownames_to_column()

count(eszek3, derivational, nsyl) # this will always be a crapshoot for the endings
eszek3

cselekszek2 = cselekszek %>% 
  filter(is.na(Petya),is.na(Ági)) %>% 
  rename(word = prompt) %>% 
  group_by(nsyl) %>% 
  sample_n(list_length / 2) %>% 
  ungroup() %>%  
  select(word,nsyl,suffix,nek,tek,ünk) %>% 
  rename(derivational = suffix) %>% 
  sample_n(n()) %>% 
  arrange(nsyl,derivational) %>%
  mutate(
    cc_prompt = str_extract(word, '^.*(?= /)'),
    cvc_prompt = str_extract(nek, '(?<=/ ).*$'),
    cvc_prompt = str_replace(cvc_prompt, '.k$', 'i'),
    keep = rep(c('nek','tek','ünk'),list_length/3)
    ) %>% 
  pivot_longer(-c(word,cc_prompt,cvc_prompt,nsyl,derivational,keep), names_to = 'suffix') %>% 
  filter(keep == suffix)

cselekszek3 = cselekszek2 %>% 
  mutate(
    prompt = glue('{cc_prompt}, {cvc_prompt}'),
    target1 = str_extract(value, '^.*(?= /)'),
    target2 = str_extract(value, '(?<=/ ).*$'),
    category = 'cselekszik',
    target_sentence = case_when(
      suffix == 'nek' ~ 'Ők ritkán...',
      suffix == 'tek' ~ 'Ti ritkán...',
      suffix == 'ünk' ~ 'Mi ritkán...'
    ),
    carrier_sentence = glue('János imád {cvc_prompt}, ezért sokat {cc_prompt}.')
  ) %>% 
  select(word,prompt,suffix,nsyl,derivational,carrier_sentence,target_sentence,target1,target2,category) %>% 
  sample_n(n()) %>% 
  rownames_to_column()

count(cselekszek3, nsyl, derivational, suffix)
count(cselekszek3, nsyl, suffix)

# -- sample into participant lists -- #

list1 = bind_rows(
  filter(dzsungel3, rowname %in% 1:(list_length/3)),
  filter(eszek3, rowname %in% 1:(list_length/3)),
  filter(cselekszek3, rowname %in% 1:(list_length/3))
)

list2 = bind_rows(
  filter(dzsungel3, rowname %in% (list_length/3+1):(list_length/3*2)),
  filter(eszek3, rowname %in% (list_length/3+1):(list_length/3*2)),
  filter(cselekszek3, rowname %in% (list_length/3+1):(list_length/3*2))
)

list3 = bind_rows(
  filter(dzsungel3, rowname %in% (list_length/3*2+1):list_length),
  filter(eszek3, rowname %in% (list_length/3*2+1):list_length),
  filter(cselekszek3, rowname %in% (list_length/3*2+1):list_length)
)

count(list1, category, suffix, nsyl, vowel)
count(list2, category, suffix, nsyl, vowel)
count(list3, category, suffix, nsyl, vowel)

# mindegy jó lesz

# -- trial tsv -- #

write_csv(list1, '~/Github/Pavlovia/hesp_baseline/list1.csv')
write_csv(list2, '~/Github/Pavlovia/hesp_baseline/list2.csv')
write_csv(list3, '~/Github/Pavlovia/hesp_baseline/list3.csv')
