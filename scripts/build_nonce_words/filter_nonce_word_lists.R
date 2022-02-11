## filter nonce words: no words too close to real words, no overlaps at beginning w/ real words, no words too close to each other

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)
library(furrr)

#library(progress)
library(tictoc)

tic('script took this long')

# -- read -- #

h = read_tsv('src/hu_list.txt')
stems = read_tsv('src/nonce_words/stems/nonce_stems.tsv')

# -- functions -- #

# match individual nonce word against spelling dictionary. return T if it has an edit distance of 1 or 0 with any word on list OR overlaps with ^any word on list. else return F.
matchReal = function(w,h){
  #pb$tick()
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
  #pb$tick()
  crossing(
    word = dat,
    match = dat
  ) %>% 
    filter(word != match) %>% 
    mutate(
      dist = stringdist::stringdist(word, match, method = 'lv')
    ) %>% 
    group_by(word) %>% 
    summarise(min_dist = min(dist)) %>% 
    filter(min_dist > 1) %>% 
    select(word)
}

# -- wrangle -- #

h %<>%
  filter(nchar(word) > 3)

# -- filter for real words -- #

# future::nbrOfWorkers(evaluator = NULL)
plan(multisession, workers = 8)

stems2 = stems[1:2,]

# do only for nsyl>1 !
stems2 = stems2 %>% 
  rowwise() %>% 
  mutate(
    pass = map_lgl(word, ~ matchReal(.,h))
  )

stems3 = filter(stems2, pass | nsyl == 1)

# -- filter for nonce words -- #

keep_words = matchNonce(stems3$word)

stems4 = filter(stems3, word %in% keep_words)

# -- write -- #

write_tsv(stems4, 'src/nonce_words/stems/nonce_stems_filt.tsv')

toc(log = T)
glue('We thank you for your valuable time.')
