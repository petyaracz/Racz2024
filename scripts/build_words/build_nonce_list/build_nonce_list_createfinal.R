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

# randomise form order

n %<>% sample_n(n())
v1 %<>% sample_n(n())
v2 %<>% sample_n(n())

# first half of each set goes into eszek/eszem. second half to cselekedik/cselekszik
v1a = v1 %>% head(round(nrow(v1)/2))
v1b = v1 %>% tail(round(nrow(v1)/2))
v2a = v2 %>% head(round(nrow(v2)/2))
v2b = v2 %>% tail(round(nrow(v2)/2))

## nouns

# we want these suffixes
suffix_back = c('ban', 'ba', 'nak', 'ra', 'nál')

# we combine stems with suffixes, generate the two alternatives.
n2 = n %>% 
  mutate(vowel_e = str_detect(coda, 'e')) %>% 
  select(word,tr,vowel_e) %>% 
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
  mutate(nsyl = 1) %>% 
  select(v,word,nsyl) %>% 
  rename('prompt' = word)

# 2syl
v2a2 = v2a %>% 
  mutate(nsyl = 2) %>% 
  rename('prompt' = word) %>% 
  mutate(v = str_extract(v2c3c4, '[aáeéiíoóöőuúüű]')) %>% 
  select(v,prompt,nsyl)

# 1sg indef -k/-m requires a linking vowel that agrees in front and round with stem.    
v_ik = bind_rows(v1a2,v2a2) %>% 
  mutate(
    suffix_v = case_when(
      str_detect(v, '[aáiíoóuú]') ~ 'o',
      str_detect(v, '[üűöő]') ~ 'ö',
      str_detect(v, '[eé]') ~ 'e'
    ),
    target_k = str_replace(prompt, 'ik', glue('{suffix_v}k')),
    target_m = str_replace(prompt, 'ik', glue('{suffix_v}m')),
    tr = transcribe(prompt, 'single')
  ) %>% 
  select(prompt,tr,nsyl,target_k,target_m)

# ep

# we take possible endings from real ep verbs: think alszik -> a-lsza-na and a-lusz-na. what are the existing combinations of consonants and vowels here?
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
    ending = glue('{c1}(V){c2}/{c3}')
  ) %>% 
  distinct(c1,c2,c3,ending) %>% 
  filter(!(c2 == 'ß' & c3 == 'ß')) # we only want aludtok / alszotok, emlékeztek / emlékszetek, not alusztok / alszotok. otherwise lot of similar forms in final, and c alteration is cooler.

# these are the possible combinations of c1c2-c1Vc3 that exist. some cc forms go with linking vowels that are not a or e by default. this is because these are lowering stems / the suffixes have something funky going on.
# so we fix linking vowels as a and e for now.
# and then change them later when necessary.
# it won't be pretty.

# ep_pieces
# "u" "o" "ö" "e" "ü" "a"

# ep %>% 
#   sample_n(n()) %>% 
#   group_by(xpostag) %>% 
#   slice_head(n = 1) %>% 
#   select(stem,form_1,form_2,xpostag)

# we want these suffixes
suffix_ep = c('na', 'ni', 'tok', 'nak', 'tak')

# 1syl stems
v1b2 = v1b %>% 
  mutate(
    nsyl = 1,
    stem = glue('{onset}{v}')
  ) %>% 
  select(stem,v,nsyl)

# 2syl stems
v2b2 = v2b %>% 
  filter(nchar(v1c1c2) < 3) %>% # one CC cluster in a word is more than enough.
  mutate(
    nsyl = 2,
    v = str_extract(v2c3c4, '^.'),
    stem = glue('{onset}{v1c1c2}{v}')
  ) %>% 
  select(stem,v,nsyl) 

# bind stems. 
# cross with possible ep endings, giving us every combination in the world.
# build vowels
# create various stems: cc stem, cvc stem. also w/o ik. that might not be a viable form in itself, but we need it for endings. think: bomol, *boml, bomlik, *bomolik, but boml-anak, bomol-na.
v_ep_1 = bind_rows(v1b2,v2b2) %>% 
  sample_n(n()) %>% 
  crossing(ep_pieces) %>% 
  mutate(
    epv = case_when(
      str_detect(v, '[aáoóuú]') ~ 'o',
      str_detect(v, '[eéií]') ~ 'e',
      str_detect(v, '[öőüű]') ~ 'ö'
    ),
    lv = case_when(
      str_detect(v, '[aáoóuú]') ~ 'a',
      str_detect(v, '[eéiíöőüű]') ~ 'e'
    ),
    prompt_cc_tr = glue('{stem}{c1}{c2}ik'),
    prompt_cvc_tr = glue('{stem}{c1}{epv}{c3}ik'),
    prompt_cc = transcribe(prompt_cc_tr, 'double'),
    prompt_cvc = transcribe(prompt_cvc_tr, 'double'),
    prompt_cvc_bare = str_replace(prompt_cvc, 'ik', ''),
    prompt_cc_bare = str_replace(prompt_cc, 'ik', lv)
  ) %>% 
  select(stem,prompt_cc_tr,nsyl,prompt_cc,prompt_cvc,prompt_cc_bare,prompt_cvc_bare,epv,lv,ending)

# we do some common-sense filtering here as well.
v_ep_1 %<>% 
  filter(
    str_count(prompt_cc, 'y') <= 1,
    str_count(prompt_cc, 's') <= 1,
    str_count(prompt_cc, 'z') <= 1
  )

# this is a lot of similar looking forms. we'll sample them down later.

# NOW we combine these with the suffixes. the way we did with the nouns (except there the stems were a given).
# there's some THIS IS X EXCEPT WHEN IT ISN'T code here. could be done nicer.
# suffix vowel is usually front/back sometimes also round (so a/e/o/ö)
# linking vowel is a/e except for tok (alszotok, *alszatok, alusztok)
# tak lengthens t after linking vowel: áramoltak, áramlottak
# of course charmingly enough tok and tak are the same for cc front stems: ti lélegeztek (you breath), ők lélegeztek (they breathed). but due to above, not for cvc forms: ti lélegzetek, ők lélegzettek.

# I should parallellise this at some point:
v_ep_2 = v_ep_1 %>% 
  sample_n(n()) %>% 
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
    target_cc = case_when(
      suffix_ep != 'tak' ~ glue('{prompt_cc_bare}{suffix_ep_f}'),
      suffix_ep == 'tak' ~ glue('{prompt_cc_bare}t{suffix_ep_f}')
    ),
    target_cc = target_cc %>% 
      str_replace_all( c('.(?=ttak)' = 'o', '.(?=tok)' = 'o', '.(?=ttek)' = 'ö', '.(?=tök)' = 'ö')), # cf ők játszottak, ti játszotok, ők döglöttek, ti dögöltök
    target_cvc = glue('{prompt_cvc_bare}{suffix_ep_f}')
  ) %>%
  select(prompt_cc,prompt_cvc,suffix_ep,target_cc,target_cvc,stem,prompt_cc_tr,nsyl,ending)

# count(v_ep_2,suffix_ep)

# -- winnow -- #

n_keep = n2 %>% 
  sample_n(n()) %>% 
  distinct(prompt,vowel_e) %>% 
  group_by(vowel_e) %>% 
  sample_n(1024) %>% 
  pull(prompt)

ep_keep = v_ep_2 %>% 
  distinct(nsyl,stem,ending,prompt_cc,prompt_cvc) %>% 
  group_by(nsyl,ending) %>% 
  sample_n(48) %>% 
  pull(prompt_cc)

v_ik2 = v_ik %>% 
  group_by(nsyl) %>% 
  sample_n(1024) %>% 
  ungroup()

n3 = n2 %>% 
  filter(prompt %in% n_keep)

v_ep_3 = v_ep_2 %>% 
  filter(prompt_cc %in% ep_keep)

# -- write -- #

# dzsungelban
write_tsv(n2, 'resource/nonce_words/nouns_unfilt.tsv.gz')
write_tsv(n3, 'resource/nonce_words/nouns_unfilt_sample.tsv')
# eszek
write_tsv(v_ik, 'resource/nonce_words/ik_unfilt.tsv.gz')
write_tsv(v_ik2, 'resource/nonce_words/ik_unfilt_sample.tsv')
# cselekszik
write_tsv(v_ep_2, 'resource/nonce_words/ep_unfilt.tsv.gz')
write_tsv(v_ep_3, 'resource/nonce_words/ep_unfilt_sample.tsv')
