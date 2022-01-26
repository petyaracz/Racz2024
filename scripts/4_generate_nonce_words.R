## generate nonce words from the three sets

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(magrittr)

library(ngram)

# -- read -- #

ik = read_tsv('src/ik_verbs/ikes_pairs_webcorpus2.tsv')
vh = read_tsv('src/front_harmony/fh_pairs_webcorpus2.tsv')
ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- pre-processing -- #

#...

# -- make words -- #

seed = vh %>% 
  pull(stem) %>%
  # str_replace('[oeö]$', '') %>% 
  str_split('') %>% 
  map_chr( ~ paste(., collapse = ' '))

my_2gram = ngram(seed, n = 2, sep = ' ')
my_3gram = ngram(seed[nchar(seed)>=6], n = 3, sep = ' ')
my_4gram = ngram(seed[nchar(seed)>=8], n = 4, sep = ' ')

babble(my_2gram, genlen = round(runif(1,4,8)), seed = round(runif(1,1,1000)))
babble(my_3gram, genlen = round(runif(1,4,8)), seed = round(runif(1,1,1000)))
babble(my_4gram, genlen = round(runif(1,4,8)), seed = round(runif(1,1,1000)))
