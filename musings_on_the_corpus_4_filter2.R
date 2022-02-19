# filter for itself.

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

library(furrr)

# -- functions -- #

# match words against themselves, within each set. drop word if any other word is closer than 1 distance. technically this removes both members of such a pair and is overkill.
matchNonce = function(words){
  formz = crossing(
    tr = words,
    match = words
  ) %>%
    filter(tr != match) %>%
    mutate(
      dist = stringdist::stringdist(tr, match, method = 'lv')
    )

  formz %<>%
    group_by(tr) %>%
    summarise(min_dist = min(dist))

  formz %>% 
    filter(min_dist > 1) %>%
    pull(tr)
}

# -- read -- #

n = read_tsv('n_filt.tsv')
v1 = read_tsv('vs_filt.tsv')
v2 = read_tsv('vl_filt.tsv')

# -- wrangle -- #

n2 = n %>% 
  filter(keep_enough_distance,keep_no_overlap) %>% 
  pull(tr) %>% 
  matchNonce()

n = n %>% 
  filter(tr %in% n2)

v12 = v1 %>% 
  filter(keep_enough_distance,keep_no_overlap) %>% 
  pull(tr) %>% 
  matchNonce()

v1 = v1 %>% 
  filter(tr %in% v12)

v22 = v2 %>% 
  filter(keep_enough_distance,keep_no_overlap) %>% 
  pull(tr) %>% 
  matchNonce()

v2 = v2 %>% 
  filter(tr %in% v22)
