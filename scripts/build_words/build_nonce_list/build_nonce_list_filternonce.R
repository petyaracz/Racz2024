####################################
# filter words based on similarity to real words, each other
# for more info: build_nonce_list_functions
####################################

# -- header -- #

set.seed(1989)

setwd('~/Github/Racz2024/')

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

h = read_tsv('resource/hu_list.txt')
n = read_tsv('resource/nonce_words/nouns_unfilt_sample.tsv') # nonce words built from real words
ik = read_tsv('resource/nonce_words/ik_unfilt_sample.tsv')
ep = read_tsv('resource/nonce_words/ep_unfilt_sample.tsv')

# -- setup -- #

# sets to filter
nouns = distinct(n,tr)
ik_short_verbs = filter(ik, nsyl == 1) %>% 
  select(tr)
ik_long_verbs = filter(ik, nsyl == 2) %>% 
  select(tr)
ep_short_verbs = ep %>% 
  mutate(nsyl = str_count(stem, '[aáeéiíoóöőuúüű]')) %>% 
  filter(nsyl == 1) %>% 
  distinct(prompt_cc_tr) %>% 
  mutate(tr = prompt_cc_tr)
ep_long_verbs = ep %>% 
  mutate(nsyl = str_count(stem, '[aáeéiíoóöőuúüű]')) %>% 
  filter(nsyl == 2) %>% 
  distinct(prompt_cc_tr) %>% 
  mutate(tr = prompt_cc_tr)

# comparison sets
h_n_vl = h %>% 
  filter(nchar(tr) > 2, nchar(tr) <= 7) %>% 
  pull(tr)
h_vs = h %>% 
  filter(nchar(tr) > 2, nchar(tr) <= 7) %>% 
  pull(tr)

# relevant sets for margin search (short existing words)
h_margins = h %>% 
  filter(nchar(tr) >= 2, nchar(tr) <= 5) %>% 
  pull(tr)

# -- try -- #

# nouns = nouns[1:2,]
# ik_short_verbs = ik_short_verbs[1:2,]
# ep_short_verbs = ep_short_verbs[1:2,]
# ik_long_verbs = ik_long_verbs[1:2,]
# ep_long_verbs = ep_long_verbs[1:2,]

# -- filter -- #

# we use furrr to parallelise code. 
plan(multisession, workers = 8)

# n
glue('doing nouns...')

# map through nouns. check for distance and overlap. create boolean cols. time it.
tictoc::tic('nouns')
nouns2 = matchWrapper(d = nouns, h = h_n_vl, h_margins = h_margins, dist = 2)
tictoc::toc()

## v1
glue('doing short verbs...')
# map and time monosyl verbs.
tictoc::tic('ik short verbs')
ik_short_verbs2 = matchWrapper(d = ik_short_verbs, h = h_vs, h_margins = h_margins, dist = 1)
tictoc::toc()
tictoc::tic('ep short verbs')
ep_short_verbs2 = matchWrapper(d = ep_short_verbs, h = h_vs, h_margins = h_margins, dist = 1)
tictoc::toc()

## v2
glue('doing long verbs...')
# map and time bisyllabic verbs
tictoc::tic('ik long verbs')
ik_long_verbs2 = matchWrapper(d = ik_long_verbs, h = h_n_vl, h_margins = h_margins, dist = 2)
tictoc::toc()
tictoc::tic('ep long verbs')
ep_long_verbs2 = matchWrapper(d = ep_long_verbs, h = h_n_vl, h_margins = h_margins, dist = 2)
tictoc::toc()

glue('doing within-set similarity...')
## filtering based on similarity to other nonce words in group
nouns3 = similarityWrapper(nouns2)
ik_short_verbs3 = similarityWrapper(ik_short_verbs2)
ep_short_verbs3 = similarityWrapper(ep_short_verbs2)
ik_long_verbs3 = similarityWrapper(ik_long_verbs2)
ep_long_verbs3 = similarityWrapper(ep_long_verbs2)

# -- recombine -- #
n2 = right_join(nouns3,n)
ik2 = bind_rows(ik_short_verbs3,ik_long_verbs3) %>% 
  right_join(ik,.)
ep2 = bind_rows(ep_short_verbs3,ep_long_verbs3) %>% 
  right_join(ep,.)

# -- write -- #
glue('writing...')

write_tsv(n2, 'resource/nonce_words/nouns_filt_sample.tsv')
write_tsv(ik2, 'resource/nonce_words/ik_filt_sample.tsv')
write_tsv(ep2, 'resource/nonce_words/ep_filt_sample.tsv')

glue('We thank you for your valuable time!')