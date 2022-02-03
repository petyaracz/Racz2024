## filter nonce words: no words too close, no overlaps at beginning

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
# ep = sample_n(ep, n())

ik = ik[1:2500,]
vh = vh[1:2500,]
# ep = ep[1:2500,]

# -- functions -- #

# match individual nonce word against spelling dictionary. return T if it has an edit distance of 1 or 0 with any word on list OR overlaps with ^any word on list. else return F.
matchNonce = function(w,h){
  dists = map(h, ~ stringdist::stringdist(., w, method = 'lv')) %>% 
    unlist()
  beginnings = map(h, ~ str_detect(w, glue('^{.}'))) %>% 
    unlist()
  pass1 = 1 < min(dists)
  pass2 = !any(beginnings)
  
  pass1 & pass2
}

# -- wrangle -- #

h %<>%
  filter(nchar(word) > 2)

# -- filter -- #

# future::nbrOfWorkers(evaluator = NULL)
plan(multisession, workers = 8)

ik2 = future_map(ik$word, ~ matchNonce(.,h))
vh2 = future_map(vh$word, ~ matchNonce(.,h))
ep2 = future_map(ep$word, ~ matchNonce(.,h))

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

# -- write -- #

ik %>% 
  select(word) %>% 
  write_tsv('nonce_words/filt/nonce_ik_filt.txt')

vh %>% 
  select(word) %>% 
  write_tsv('nonce_words/filt/nonce_vh_filt.txt')

ep %>% 
  select(cvc,cc) %>% 
  write_tsv('nonce_words/filt/nonce_ep_filt.txt')


