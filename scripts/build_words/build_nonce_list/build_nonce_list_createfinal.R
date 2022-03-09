####################################
# build final forms
# for more info: build_nonce_list_functions
####################################

# -- header -- #

setwd('~/Github/Racz2024/')

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

h = read_tsv('resource/hu_list.txt')
n = read_tsv('resource/nonce_words/nounstems.tsv', na = character()) # filtered nonce word lists
v1 = read_tsv('resource/nonce_words/verbsshortstems.tsv', na = character())
v2 = read_tsv('resource/nonce_words/verbslongstems.tsv', na = character())
ep = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv') # real epenthetic verbs for reference

# -- wrangle -- #

## nouns

# we want these suffixes
suffix_back = c('ban','nak','nál')

# we combine stems with suffixes, generate the two alternatives.
n2 = n %>% 
  mutate(vowel_e = str_detect(coda, 'e')) %>% 
  select(word,tr,vowel_e) %>% 
  rename('prompt' = word) %>% 
  crossing(suffix_back) %>% 
  mutate(
    suffix_front = str_replace_all(suffix_back, c('ban$' = 'ben', 'nak' = 'nek', 'nál$' = 'nél')),
    target_front = glue('{prompt}{suffix_front}'),
    target_back = glue('{prompt}{suffix_back}')
  )

# sample base forms
n2 = n %>% 
  sample_n(n()) %>% 
  select(tr) %>% 
  left_join(n2, by = 'tr')

## verbs

# split word sets. we need more fuel for the ep words

v1l = v1 %>% 
  sample_n(n()) %>% 
  rownames_to_column() %>% 
  mutate(three = as.double(rowname) %% 3 == 0) %>% 
  group_split(three)

v2l = v2 %>% 
  sample_n(n()) %>% 
  rownames_to_column() %>% 
  mutate(three = as.double(rowname) %% 3 == 0) %>% 
  group_split(three)

# ep

# we take possible endings from real ep verbs: think alszik -> a-lsza-na and a-lud-na. what are the existing combinations of consonants and vowels here? what are the pairs where c2 != c3? like above
# form1 = alszanak/ form2 = aludnak: c1 = l, c2 = sz, c3 = d, epv = u, lv = a
ep_pieces = ep %>%
  mutate(
    form_1_tr = transcribe(form_1, 'single'),
    form_2_tr = transcribe(form_2, 'single'),
    stem_tr = transcribe(stem, 'single'),
    c1 = str_extract(stem_tr, '.$'),
    c2 = str_extract(form_1_tr, glue('(?<={stem_tr}).')),
    epv = str_extract(form_2_tr, glue('(?<={stem_tr}).')),
    c3 = str_extract(form_2_tr, glue('(?<={stem_tr}{epv}).')),
    lv = str_extract(form_1_tr, glue('(?<={stem_tr}{c2}).')),
    # lv = case_when(
    #   str_detect(epv, '[aáoóuú]') ~ 'a',
    #   str_detect(epv, '[eéiíöőüű]') ~ 'e'
    # ),
    c1c2 = glue('{c1}{c2}'),
    ending = glue('{c1}(V){c2}/{c3}')
  ) %>% 
  distinct(c1,c2,c3,ending,c1c2) 

# all of these look like sort of derivational suffixes (zik, lik, ßik), except rik.
ep_pieces %<>% 
  filter(str_detect(c2, '[zßld]'))

# ep_pieces %>% 
#   distinct(c2,c3) %>% 
#   filter(c2 != c3)
# ß/d ß/z l/d

# we want these suffixes
suffix_ep = c('ünk', 'tek', 'nek')

# 1syl stems
v1_ep = v1l[[1]] %>% 
  mutate(
    nsyl = 1
  ) %>% 
  filter(
    c1c2 %in% ep_pieces$c1c2
  )

# 2syl stems
v2_ep = v2l[[1]] %>% 
  mutate(
    cc = str_extract(tr, '..(?=ik$)'),
    nsyl = 2,
    v = str_extract(v2c3c4, '^.'),
    vowel_skeleton = str_extract_all(tr, '[aáeéiíoóöőuúüű]') %>% 
      unlist() %>% 
      paste(collapse = '')
  ) %>% 
  filter(
    nchar(v1c1c2) < 3, # one CC cluster in a word is more than enough.
    nchar(v2c3c4) == 3, # we need vccik.
    cc %in% ep_pieces$c1c2
  )

# bind stems. 
# cross with possible ep endings, giving us every combination in the world.
# build vowels
# create various stems: cc stem, cvc stem. also w/o ik. that might not be a viable form in itself, but we need it for endings. think: bomol, *boml, bomlik, *bomolik, but boml-anak, bomol-na.
v_ep_1 = bind_rows(v1_ep,v2_ep) %>% 
  mutate(
    epv = case_when(
      str_detect(v, '[aáoóuú]') ~ 'o',
      str_detect(v, '[eé]') ~ 'e',
      str_detect(v, '[öőüű]') ~ 'ö',
      nsyl == 1 & str_detect(v, '[ií]') ~ 'o',
      str_detect(vowel_skeleton, '[aáoóuú][ií]') ~ 'o',
      str_detect(vowel_skeleton, '[eéöőüű][ií]') ~ 'e'
    ),
    lv = case_when(
      str_detect(v, '[aáoóuú]') ~ 'a',
      str_detect(v, '[eéöőüű]') ~ 'e',
      nsyl == 1 & str_detect(v, '[ií]') ~ 'a',
      str_detect(vowel_skeleton, '[aáoóuú][ií]') ~ 'a',
      str_detect(vowel_skeleton, '[eéöőüű][ií]') ~ 'e'
    ),
    tr_end = str_extract(tr, '.ik$'),
    tr_stem = str_extract(tr, glue('^.*(?={tr_end})')),
    prompt_cc_tr = tr,
    prompt_cvc_tr = glue('{tr_stem}{epv}{tr_end}'),
    prompt_cc = transcribe(prompt_cc_tr, 'double'),
    prompt_cvc = transcribe(prompt_cvc_tr, 'double'),
    prompt_cvc_bare = str_replace(prompt_cvc, 'ik$', ''),
    prompt_cc_bare = str_replace(prompt_cc, 'ik$', lv)
  ) %>% 
  select(tr_stem,tr_end,prompt_cc_tr,nsyl,prompt_cc,prompt_cvc,prompt_cc_bare,prompt_cvc_bare,epv,lv)

# we do some common-sense filtering here as well.
v_ep_1 %<>% 
  filter(
    str_count(prompt_cc, 'y') <= 1,
    str_count(prompt_cc, 's') <= 1,
    str_count(prompt_cc, 'z') <= 1
  )

# we want to spice it up: so ß will vary with d and l will vary with z
v_ep_1 %<>% 
  mutate(
    prompt_cvc_bare = str_replace_all(
      prompt_cvc_bare, c('sz$' = 'd', 'l$' = 'z')
    ),
    prompt_cvc = glue('{prompt_cvc_bare}ik') # yes we build it back
  )

# NOW we combine these with the suffixes. the way we did with the nouns (except there the stems were a given).
# there's some THIS IS X EXCEPT WHEN IT ISN'T code here. could be done nicer.
# suffix vowel is usually front/back sometimes also round (so a/e/o/ö)
# linking vowel is a/e except for tok (alszotok, *alszatok, alusztok)
# tak lengthens t after linking vowel: áramoltak, áramlottak
# of course charmingly enough tok and tak are the same for cc front stems: ti lélegeztek (you breath), ők lélegeztek (they breathed). but due to above, not for cvc forms: ti lélegzetek, ők lélegzettek.

v_ep_2 = v_ep_1 %>% 
  crossing(suffix_ep) %>%  
  mutate(
    suffix_ep_vh = case_when(
      str_detect(epv, '[aáiíoóuú]') & suffix_ep == 'nek' ~ 'a',
      str_detect(epv, '[eéöőüű]') & suffix_ep == 'nek' ~ 'e',
      str_detect(epv, '[aáiíoóuú]') & suffix_ep == 'ünk' ~ 'u',
      str_detect(epv, '[eéöőüű]') & suffix_ep == 'ünk' ~ 'ü',
      str_detect(epv, '[aáií]') & suffix_ep == 'tek' ~ 'a',
      str_detect(epv, '[oóuú]') & suffix_ep == 'tek' ~ 'o',
      str_detect(epv, '[eé]') & suffix_ep == 'tek' ~ 'e',
      str_detect(epv, '[öőüű]') & suffix_ep == 'tek' ~ 'ö'
      ),
    suffix_ep_f = str_replace(suffix_ep, '[eü]', suffix_ep_vh),
    target_cc = glue('{prompt_cc_bare}{suffix_ep_f}') %>% 
      str_replace_all(
        c('.(?=[uü]nk$)' = '', 'atok$' = 'otok', 'etök$' = 'ötök')
        ),
    target_cvc = glue('{prompt_cvc_bare}{suffix_ep_f}')
  ) %>%
  select(prompt_cc,prompt_cvc,suffix_ep,target_cc,target_cvc,tr_stem,tr_end,prompt_cc_tr,nsyl)

v_ep_2 = v_ep_1 %>% 
  sample_n(n()) %>% 
  select(prompt_cc,prompt_cvc) %>% 
  left_join(v_ep_2)

# ik

# take eszek/eszem, figure out linking vowel, generate targets
# 1syl
v1_ik = v1l[[2]] %>% 
  mutate(nsyl = 1) %>% 
  select(v,word,nsyl) %>% 
  rename('prompt' = word)

# 2syl
v2_ik = v2l[[2]] %>% 
  mutate(nsyl = 2) %>% 
  rename('prompt' = word) %>% 
  mutate(v = str_extract(v2c3c4, '[aáeéiíoóöőuúüű]')) %>% 
  select(v,prompt,nsyl)

# 1sg indef -k/-m requires a linking vowel that agrees in front and round with stem.    
v_ik = bind_rows(v1_ik,v2_ik) %>% 
  mutate(
    suffix_v = case_when(
      str_detect(v, '[aáiíoóuú]') ~ 'o',
      str_detect(v, '[üűöő]') ~ 'ö',
      str_detect(v, '[eé]') ~ 'e'
    ),
    target_k = str_replace(prompt, 'ik', glue('{suffix_v}k')),
    target_m = str_replace(prompt, 'ik', glue('{suffix_v}m')),
    tr = transcribe(prompt, 'single'),
    ending = str_extract(prompt, '.ik$')
  ) %>% 
  select(prompt,tr,nsyl,ending,target_k,target_m)

# -- get sufficiently distant nonce words in sets -- #

nk = n2 %>% 
  distinct(tr,vowel_e) %>% 
  group_by(vowel_e) %>% 
  sample_n(500) %>% 
  nest() %>% 
  mutate(subset = map(data, ~ matchNonce(.$tr,1))) %>% 
  select(-data) %>% 
  unnest(cols = c(subset)) %>% 
  filter(keep_self_diff)

# count(n3,vowel_e)

v_ikk = v_ik %>% 
  mutate(ending = str_extract(tr, '.ik$')) %>% 
  filter(ending != 'dik') %>% 
  distinct(tr,nsyl,ending) %>% 
  group_by(nsyl,ending) %>% 
  nest() %>% 
  mutate(subset = map(data, ~ matchNonce(.$tr,1))) %>% 
  select(-data) %>% 
  unnest(cols = c(subset)) %>% 
  filter(keep_self_diff)

# count(v_ik2,nsyl,ending)

v_epk = v_ep_2 %>% 
  filter(tr_end != 'dik') %>% 
  distinct(prompt_cc_tr,nsyl,tr_end) %>% 
  group_by(nsyl,tr_end) %>% 
  nest() %>% 
  mutate(subset = map(data, ~ matchNonce(.$prompt_cc_tr,1))) %>% 
  select(-data) %>% 
  unnest(cols = c(subset)) %>% 
  filter(keep_self_diff)

# count(v_ep_3,nsyl,tr_end)

n3 = n2 %>% 
  filter(tr %in% nk$tr)
v_ik2 = v_ik %>%   
  filter(tr %in% v_ikk$tr)
v_ep_3 = v_ep_2 %>% 
  filter(prompt_cc_tr %in% v_epk$tr)

# -- write -- #

# dzsungelban
write_tsv(n2, 'resource/nonce_words/nouns_unfilt.tsv.gz')
# eszek
write_tsv(v_ik, 'resource/nonce_words/ik_unfilt.tsv.gz')
# cselekszik
write_tsv(v_ep_2, 'resource/nonce_words/ep_unfilt.tsv.gz')
# filtered
write_tsv(n3, 'resource/nonce_words/nouns_unfilt_sample.tsv')
write_tsv(v_ik2, 'resource/nonce_words/ik_unfilt_sample.tsv')
write_tsv(v_ep_3, 'resource/nonce_words/ep_unfilt_sample.tsv')
