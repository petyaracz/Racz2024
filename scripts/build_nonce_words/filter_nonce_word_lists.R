## filter nonce words: no words too close to real words, no overlaps at beginning w/ real words, no words too close to each other

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)
library(furrr)

# -- read -- #

h = read_tsv('src/hu_list.txt')

ik = read_tsv('nonce_words/raw/nonce_ik.txt')
vh = read_tsv('nonce_words/raw/nonce_vh.txt')
ep = read_tsv('nonce_words/raw/nonce_ep.txt')

ik = sample_n(ik, n())
vh = sample_n(vh, n())
ep = sample_n(ep, n())

# ik = ik[1:2500,]
# vh = vh[1:2500,]
# ep = ep[1:2500,]

# -- functions -- #

# match individual nonce word against spelling dictionary. return T if it has an edit distance of 1 or 0 with any word on list OR overlaps with ^any word on list. else return F.
matchReal = function(w,h){
  dists = map(h, ~ stringdist::stringdist(., w, method = 'lv')) %>% 
    unlist()
  beginnings = map(h, ~ str_detect(w, glue('^{.}'))) %>% 
    unlist()
  pass1 = 1 < min(dists)
  pass2 = !any(beginnings)
  
  pass1 & pass2
}

# match words against themselves, within each set. drop word if any other word is closer than 1 distance. technically this removes both members of such a pair and is overkill.
matchNonce = function(dat){
  crossing(
    word = dat$word,
    match = dat$word
  ) %>% 
    filter(target != match) %>% 
    mutate(
      dist = stringdist::stringdist(target, match, method = 'lv')
    ) %>% 
    group_by(target) %>% 
    summarise(min_dist = min(dist)) %>% 
    filter(min_dist > 1) %>% 
    select(word)
}

# -- wrangle -- #

h %<>%
  filter(nchar(word) > 2)

# -- filter against real words -- #

# future::nbrOfWorkers(evaluator = NULL)
plan(multisession, workers = 8)

ik2 = future_map(ik$word, ~ matchReal(.,h[1:2,]))
vh2 = future_map(vh$word, ~ matchReal(.,h))
ep2 = future_map(ep$word, ~ matchReal(.,h))

ik$pass = unlist(ik2)
vh$pass = unlist(vh2)
ep$pass = unlist(ep2)

ik %<>% filter(pass)
vh %<>% filter(pass)
ep %<>% # this is slightly more complicated because we have pairs.
  rownames_to_column() %>% 
  mutate(
    rowname = as.double(rowname),
    word_id = case_when(
      rowname %% 2 == 0 ~ rowname - 1, # science
      rowname %% 2 == 1 ~ rowname
    )
  ) %>%
  filter(pass) %>% 
  select(-pass) %>% 
  pivot_wider(id_cols = word_id, names_from = type, values_from = word, values_fill = 'drop value') %>% 
  filter(!(cvc == 'drop value'),!(cc == 'drop value'))

# -- filter against each other -- #

ik %<>% matchNonce()
vh %<>% matchNonce()
ep %<>% matchNonce()

# -- write -- #

ik %>% write_tsv('nonce_words/filt/nonce_ik_filt.txt')
vh %>% write_tsv('nonce_words/filt/nonce_vh_filt.txt')
ep %>% write_tsv('nonce_words/filt/nonce_ep_filt.txt')


