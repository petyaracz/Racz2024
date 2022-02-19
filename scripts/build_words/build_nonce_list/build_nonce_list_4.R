####################################
# build final forms
# for more info: build_nonce_list_functions
####################################

# -- header -- #

setwd('~/Github/Racz2024/')

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

h = read_tsv('resource/hu_list.txt')
n = read_tsv('resource/nonce_words/nouns_filt.tsv') # filtered nonce word lists
v1 = read_tsv('resource/nonce_words/verbs_short_filt.tsv')
v2 = read_tsv('resource/nonce_words/verbs_long_filt.tsv')
ep = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv') # real epenthetic verbs for reference

# -- wrangle -- #

# keep legit forms
n %<>% 
  filter(keep_enough_distance,keep_no_overlap,keep_self_diff)
v1 %<>% 
  filter(keep_enough_distance,keep_no_overlap)
v2 %<>% 
  filter(keep_enough_distance,keep_no_overlap,keep_self_diff)

# first half of each set goes into eszek/eszem. second half to cselekedik/cselekszik
v1a = v1 %>% head(round(nrow(v1)/2))
v1b = v1 %>% tail(round(nrow(v1)/2))
v2a = v2 %>% head(round(nrow(v2)/2))
v2b = v2 %>% tail(round(nrow(v2)/2))

## nouns

# we want these suffixes
suffix_back = c('ban', 'ba', 'nak', 'ra', 'nál')

# we combine stems with suffixes, generate the two alternatives, and then randomly pick a suffix for each word.
n2 = n %>% 
  select(word) %>% 
  rename('prompt' = word) %>% 
  crossing(suffix_back) %>% 
  mutate(
    suffix_front = str_replace_all(suffix_back, c('ban$' = 'ben', 'ba' = 'be', 'nak' = 'nek', 'ra$' = 're', 'nál$' = 'nél')),
    target_front = glue('{prompt}{suffix_front}'),
    target_back = glue('{prompt}{suffix_back}')
  )

## verbs

# ik

# take eszek/eszem, figure out linking vowel, generate targets
# 1syl
v1a2 = v1a %>% 
  select(v,word) %>% 
  rename('prompt' = word)
  
# 2syl
v2a2 = v2a %>% 
  rename('prompt' = word) %>% 
  mutate(v = str_extract(v2c3c4, '[aáeéiíoóöőuúüű]')) %>% 
  select(v,prompt)
    
v_ik = bind_rows(v1a2,v2a2) %>% 
  mutate(
    suffix_v = case_when(
      str_detect(v, '[aáiíoóuú]') ~ 'o',
      str_detect(v, '[üűöő]') ~ 'ö',
      str_detect(v, '[eé]') ~ 'e'
    ),
    target_k = str_replace(prompt, 'ik', glue('{suffix_v}k')),
    target_m = str_replace(prompt, 'ik', glue('{suffix_v}m'))
  ) %>% 
  select(prompt,target_k,target_m)

# ep

ep_pieces = ep %>% 
  mutate(
    form_1_tr = transcribe(form_1, 'single'),
    form_2_tr = transcribe(form_2, 'single'),
    stem_tr = transcribe(stem, 'single'),
    c1 = str_extract(stem_tr, '.$'),
    c2 = str_extract(form_1_tr, glue('(?<={stem_tr}).')),
    epv = str_extract(form_2_tr, glue('(?<={stem_tr}).')),
    c3 = str_extract(form_2_tr, glue('(?<={stem_tr}{epv}).'))
  ) %>% 
  distinct(stem,epv,c1,c2,c3) %>% 
  select(-stem)

# ep_pieces
# "u" "o" "ö" "e" "ü" "a"

# ep %>% 
#   sample_n(n()) %>% 
#   group_by(xpostag) %>% 
#   slice_head(n = 1) %>% 
#   select(stem,form_1,form_2,xpostag)

# we want these suffixes
suffix_ep = c('na', 'ni', 'tok', 'nak', 'tak')

v1b2 = v1b %>% 
  mutate(
    stem = glue('{onset}{v}') %>% 
      str_replace('NA', '')
    ) %>% 
  select(stem,v)

v2b2 = v2b %>% 
  mutate(
    v = str_extract(v2c3c4, '^.'),
    stem = glue('{onset}{v1c1c2}{v}') %>% 
      str_replace('NA', '')
  ) %>% 
  select(stem,v) 

v_ep = bind_rows(v1b2,v2b2) %>% 
  crossing(ep_pieces) %>% 
  filter(
    str_detect(stem, '[uoúó]') & str_detect(epv, '[uo]') |
      str_detect(stem, '[üöűő]') & str_detect(epv, '[üö]') |
      str_detect(stem, '[eé]') & str_detect(epv, '[e]') |
      str_detect(stem, '[aá]') & str_detect(epv, '[a]')
  ) %>% 
  mutate(
    prompt_cc_tr = glue('{stem}{c1}{c2}ik'),
    prompt_cvc_tr = glue('{stem}{c1}{epv}{c3}ik'),
    prompt_cc = transcribe(prompt_cc_tr, 'double'),
    prompt_cvc = transcribe(prompt_cvc_tr, 'double'),
    prompt_cvc_bare = str_replace(prompt_cvc, 'ik', ''),
    prompt_cc_bare = str_replace(prompt_cc, 'ik', epv)
  ) %>% 
  ungroup() %>% 
  sample_n(600) %>% # this is a lot of similar forms so
  select(prompt_cc,prompt_cvc,prompt_cc_bare,prompt_cvc_bare,epv) %>% 
  crossing(suffix_ep) %>%  
  mutate(
    suffix_ep_vh = case_when(
      str_detect(epv, '[aáiíoóuú]') & str_detect(suffix_ep, '(na|nak|tak|na)') ~ 'a',
      str_detect(epv, '[eéöőüű]') & str_detect(suffix_ep, '(na|nak|tak|na)') ~ 'e',
      str_detect(epv, '[aáiíoóuú]') & suffix_ep == 'tok' ~ 'o',
      str_detect(epv, '[öőüű]') & suffix_ep == 'tok' ~ 'ö',
      str_detect(epv, '[eéöőüű]') & suffix_ep == 'tok' ~ 'e',
      suffix_ep == 'ni' ~ 'i'
    ),
    suffix_ep_f = str_replace(suffix_ep, '[aio]', suffix_ep_vh),
    target_cc = glue('{prompt_cc_bare}{suffix_ep_f}'),
    target_cvc = glue('{prompt_cvc_bare}{suffix_ep_f}')
  ) %>% 
  select(prompt_cc,prompt_cvc,suffix_ep,target_cc,target_cvc)

# count(v_ep,suffix_ep)

# -- write -- #

# dzsungelban
write_tsv(n2, 'resource/nonce_words/nouns_final.tsv')
# eszek
write_tsv(v_ik, 'resource/nonce_words/ik_final.tsv')
# cselekszik
write_tsv(v_ep, 'resource/nonce_words/ep_final.tsv')
