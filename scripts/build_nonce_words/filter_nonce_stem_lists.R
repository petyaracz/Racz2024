## filter nonce words: no words too close to real words, no overlaps at beginning w/ real words, no words too close to each other

## previous file: build_nonce_words_list
## next file: sample_nonce_stems
## in: noun and verb stem list written in prev. hunspell spelling dict.
## out: same noun and verb stem list with booleans marking whether stems pass similarity checks.
## general notes
# this takes ages. using furrr.
# matchReal1 did weird things, I stopped using it.
# for 1 syl stems, the exclusion criteria are very strict, so no.
# for nouns, we only really need 2 syl stems.
# I use edit and levenshtein distance interchangeably to make your life ever so slightly worse.

## filter against real words:
# stem needs to be at least 2 edit distance from all words in spelling dict.
## filter against nonce words:
# stem needs to be at least 2 edit distance from its little friends

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)
library(furrr)

library(progress)
library(tictoc)

tic('script took this long')

# -- read -- #

h = read_tsv('src/hu_list.txt')
stems = read_tsv('src/nonce_words/stems/nonce_stems.tsv')

# -- functions -- #

# # match individual nonce word against spelling dictionary. return T if it overlaps at beginning with real word. else return F.
# matchReal1 = function(w,h){
#   overlaps = h %>% 
#     mutate(
#       beginning = glue('^{word}'), 
#       overlap = str_detect(w,beginning)
#     ) %>% 
#     pull(overlap)
#   !any(overlaps)
# }

# match individual nonce word against spelling dictionary. return T if it has an edit distance of 1 or 0 with any word on list.  else return F.
matchReal = function(w,h){
  dists = map(h, ~ stringdist::stringdist(., w, method = 'lv')) %>% 
    unlist()
  min(dists) > 1
}

# match words against themselves, within each set. drop word if any other word is closer than 1 distance. technically this removes both members of such a pair and is overkill.
matchNonce = function(dat){
  formz = crossing(
    word = dat,
    match = dat
  ) %>% 
    filter(word != match) %>% 
    mutate(
      dist = stringdist::stringdist(word, match, method = 'lv')
    )
  
  formz %<>% 
    group_by(word) %>% 
    summarise(min_dist = min(dist))
    
  formz %>% mutate(pass_nonce = min_dist > 1) %>% 
    select(word,pass_nonce)
}

# -- wrangle -- #

h %<>%
  filter(nchar(word) > 2)

# -- for nouns we only need 2-syl -- #

stems2 = stems %>% 
  filter(
    !(grammar_type == 'noun' & nsyl %in% c(1,3))
  )

# -- drop existing words -- #

stems2 %<>%
  filter(!(word %in% h$word))

# test
# stems2 = stems2[1:200,]

# -- filter for real words -- #

# future::nbrOfWorkers(evaluator = NULL)
plan(multisession, workers = 8)

pb = progress_bar$new(
  "  running matchReal [:bar] :percent in :elapsed",
  total = nrow(stems2), clear = FALSE, width = 60)

stems2 %<>%
  rowwise() %>% 
  mutate(
    pass_real = map_lgl(word, ~ {pb$tick(); matchReal(.,h)})
  )

# -- filter for nonce words -- #

pass_nonce = matchNonce(stems2$word)

stems2 = inner_join(stems2,pass_nonce)

stems2 %>% 
  filter(pass_real,pass_nonce) %>% 
  count(grammar_type,nsyl,front)

stems3 = stems2 %>% 
  filter(nsyl == 1 | ( pass_nonce & pass_real )) %>% 
  arrange(grammar_type,front,nsyl) %>% 
  select(-pass_real,-pass_nonce)

stems3 %>% 
  filter(grammar_type == 'noun') %>% 
  sample_n(n()) %>% 
  distinct(word,front) %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'nouns')

stems3 %>% 
  filter(grammar_type == 'verb') %>% 
  sample_n(n()) %>% 
  arrange(nsyl,front) %>% 
  distinct(word,front,nsyl) %>% 
  select(word,front,nsyl) %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'verbs')

# -- write -- #

write_tsv(stems2, 'src/nonce_words/stems/nonce_stems_filt.tsv')
write_tsv(stems3, 'src/nonce_words/stems/nonce_stems_filt_clean.tsv')

toc(log = T)
glue('We thank you for your valuable time.')
