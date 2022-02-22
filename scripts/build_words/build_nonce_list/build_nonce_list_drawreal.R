####################################
# build list of relevant forms for nouns, ik verbs
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
  )

# all ik verbs
v = c %>% 
  buildList2() %>% 
  filter(
    xpostag == '[/V][Prs.NDef.3Sg]',
    str_detect(lemma, 'ik$')
  )

# extract variable stems (back V + e/é)
n2 = n %>%
  rowwise() %>% 
  mutate(
    vowels = str_extract_all(lemma, '[aáeéiíoóöőuúüű]'),
    v1 = vowels[[1]],
    v2 = vowels[[2]]
  ) %>% 
  filter(
    str_detect(v1, '[aáoóuúií]'),
    str_detect(v2, '[eé]')
  )

# -- write -- #

write_tsv(v, 'resource/nonce_words/ik.tsv')
write_tsv(n2, 'resource/nonce_words/dzsungel.tsv')