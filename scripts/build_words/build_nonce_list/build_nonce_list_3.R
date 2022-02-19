####################################
# filter words based on similarity to real words, each other
# for more info: build_nonce_list_functions
####################################

# -- header -- #

setwd('~/Github/Racz2024/')

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

h = read_tsv('resource/hu_list.txt')
n = read_tsv('resource/nonce_words/nouns1.tsv') # nonce words built from real words
v1 = read_tsv('resource/nonce_words/verbsshort1.tsv')
v2 = read_tsv('resource/nonce_words/verbslong1.tsv')

# -- wrangle -- #

# relevant comparison set for 2syl nouns and ik verbs
h_n_v2 = h %>% 
  filter(nchar(tr) > 2, nchar(tr) <= 8) %>% 
  pull(tr)

# relevant set for monosyllabic verbs
h_v1 = h %>% 
  filter(nchar(tr) > 2, nchar(tr) <= 7) %>% 
  pull(tr)

# relevant sets for margin search (short existing words)
h_margins = h %>% 
  filter(nchar(tr) >= 2, nchar(tr) <= 5) %>% 
  pull(tr)

# randomise word order and pick the n first ones = random draw
n2 = n %>%
  sample_n(n()) %>%
  mutate(target = tr) %>%
  slice_head(n = n_sample)

v12 = v1 %>% 
  sample_n(n()) %>% 
  mutate(target = str_replace(tr, 'ik$', '')) %>% # add ik
  slice_head(n = n_sample)

v22 = v2 %>% 
  sample_n(n()) %>% 
  mutate(target = str_replace(tr, 'ik$', '')) %>% 
  slice_head(n = n_sample)

# -- filter -- #

# we use furrr to parallelise code. 
plan(multisession, workers = 8)

# n
glue('doing nouns...')

# map through nouns. check for distance and overlap. create boolean cols. time it.
tictoc::tic('nouns')
n3 = n2 %>%
  rowwise() %>%
  mutate(
    keep_enough_distance = future_map_lgl(target, ~ matchReal(., h_n_v2, 2)),
    keep_no_overlap = future_map_lgl(target, ~ matchMargins(., h_margins))
  )
tictoc::toc()

## v1
glue('doing short verbs...')

# map and time monosyl verbs.
tictoc::tic('short verbs')
v13 = v12 %>%
  rowwise() %>% 
  mutate(
    keep_enough_distance = future_map_lgl(tr, ~ matchReal(., h_v1, 1)),
    keep_no_overlap = future_map_lgl(target, ~ matchMargins(., h_margins))
  )
tictoc::toc()

## v2
glue('doing long verbs...')

# map and time bisyllabic verbs
tictoc::tic('long verbs')
v23 = v22 %>%
  rowwise() %>% 
  mutate(
    keep_enough_distance = future_map_lgl(tr, ~ matchReal(., h_n_v2, 2)),
    keep_no_overlap = future_map_lgl(target, ~ matchMargins(., h_margins))
  )
tictoc::toc()

glue('doing within-set similarity...')
# filtering based on similarity to other nonce words in group
nw = n3 %>% 
  pull(tr) %>% 
  matchNonce()

v1w = v13 %>% 
  pull(tr) %>% 
  matchNonce()

v2w = v23 %>% 
  pull(tr) %>% 
  matchNonce()

n4 = n3 %>% 
  mutate(keep_self_diff = tr %in% nw)

v14 = v13 %>% 
  mutate(keep_self_diff = tr %in% v1w)

v24 = v23 %>% 
  mutate(keep_self_diff = tr %in% v2w)

# -- write -- #
glue('writing...')

write_tsv(n4, 'resource/nonce_words/nouns_filt.tsv')
write_tsv(v14, 'resource/nonce_words/verbs_short_filt.tsv')
write_tsv(v24, 'resource/nonce_words/verbs_long_filt.tsv')

glue('We thank you for your valuable time!')