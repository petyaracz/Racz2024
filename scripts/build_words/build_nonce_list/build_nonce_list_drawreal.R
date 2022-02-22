####################################
# build list of relevant forms for nouns, ik verbs
# save broader sets for later comparison
# for more info: build_nonce_list_functions
####################################

# -- header -- #

setwd('~/Github/Racz2024/')

source('build_nonce_list_functions.R')

# -- read -- #

h = read_tsv('resource/hu_list.txt') # spelling dictionary
c = read_tsv('resource/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz') # frequency list. big.

# -- wrangle -- #

# all bisyllabic nouns
n = c %>% 
  buildList2() %>% 
  filter(
    xpostag == '[/N][Nom]',
    str_count(lemma, '[aáeéiíoóöőuúüű]') == 2
  ) %>% 
  rowwise() %>% 
  mutate(
    vowels = str_extract_all(lemma, '[aáeéiíoóöőuúüű]'),
    v1 = vowels[[1]],
    v2 = vowels[[2]]
  )

# all verbs
v = c %>% 
  buildList2() %>% 
  filter(
    xpostag == '[/V][Prs.NDef.3Sg]'
  )

# extract variable stems (back V + e/é)
n2 = n %>%
  filter(
    str_detect(v1, '[aáoóuúií]'),
    str_detect(v2, '[eé]')
  )

# extract ik verbs

v2 = v %>% 
  filter(
    str_detect(lemma, 'ik$')
  )

# comparison sets for nouns
n3 = n %>% 
  filter(
    lemma %in% h$word
  ) %>% 
  mutate(
    vh = case_when(
      str_detect(v1, '[aáoóuú]') & str_detect(v2, '[aáoóuú]') ~ 'back',
      str_detect(v1, '[eéiíöőüű]') & str_detect(v2, '[eéiíöőüű]') ~ 'front',
      T ~ 'mixed'
    )
  ) %>% 
  select(-vowels)

# comparison sets for ik verbs

v3 = v %>% 
  filter(
    lemma %in% h$word
  )

# -- write -- #

write_tsv(v2, 'resource/nonce_words/ik.tsv')
write_tsv(n2, 'resource/nonce_words/dzsungel.tsv')

write_tsv(n3, 'resource/real_words/noun_bag.tsv')
write_tsv(v3, 'resource/real_words/verb_bag.tsv')
