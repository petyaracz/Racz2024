# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)
library(magrittr)

# -- simlist -- #

# starting setup:
# 210 nouns
# 210 ep verbs
# 210 ik verbs

# one participant
# nouns: 3 suffixes x i words
# ep verbs: 3 suff x i words
# ik verbs: i words
# let i be 30

i = 210

n = tibble(
  prompt = glue('fonev{1:i}'),
  rowname = 1:i,
  type = 'n'
  ) %>% 
  crossing(
    suffix = c('ban','nak','ra')
  ) %>% 
  mutate(
    target1 = glue('{prompt}-{suffix}'),
    target2 = str_replace(target1, 'a', 'e')
  ) %>% 
  arrange(rowname)

ik = tibble(
    prompt = glue('ige{1:i}'),
    rowname = 1:i,
    type = 'ik'
  ) %>% 
  mutate(
    target1 = glue('{prompt}-ozok'),
    target2 = glue('{prompt}-ozom')
  )

ep = tibble(
    prompt = glue('ige{1:i}'),
    rowname = 1:i,
    type = 'ep'
  ) %>% 
  crossing(
    suffix = c('ünk','tek','nek')
  ) %>% 
  mutate(
    target1 = glue('{prompt}-ed{suffix}'),
    target2 = glue('{prompt}-sz{suffix}') %>% 
      str_replace_all(c('tek' = 'etek', 'nek' = 'enek'))
  ) %>% 
  arrange(rowname)

# -- split up -- #

# split up nouns, ep into 3 lists where w1-s1,w2-s2,w3-s3; w1-s2,w2-s3,w3-s1, w1-s3,w2-s1,w3-s2
# across these, split them up into seven lists
# recombine things somehow into separate lists that have a mix of everything somewhere.