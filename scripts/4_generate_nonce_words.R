## generate nonce words from the three sets

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(magrittr)

# -- functions -- #

# take vh, take it apart to constituent syllable bits, return tibble of these bits
buildVHgrammar = function(vh){
  
  vh_g = vh %>% 
    distinct(stem) %>% 
    rowwise() %>% 
    mutate(
      o1 = str_extract_all(stem, '^[^aáoóuú]*', simplify = T) %>% paste(collapse = ''),
      o2 = str_extract_all(stem, '(?<=[aáoóuú])[^aáoóuúeé]*', simplify = T) %>% paste(collapse = ''),
      o3 = str_extract_all(stem, '(?<=[eé])[^aáoóuúeé]*$', simplify = T) %>% paste(collapse = ''),
    ) %>% 
    ungroup()
}

# take vh constituent syllable bits (output of buildVHgrammar), generate word, send it through "this doesn't look hungarian" filter, keep doing that until it gets through the filter, output word
generateVH = function(vh_g){
  
  res = character(0)
  
  while ( is_empty(res) ) {
    
    ch1 = sample(vh_g$o1,1)
    ch3 = sample(vh_g$o2,1)
    ch5 = sample(vh_g$o3,1)
    ch2 = sample(c('a','á','o','ó','u','ú'),1)
    ch4 = sample(c('e','é'),1)
    res = paste0(ch1,ch2,ch3,ch4,ch5) %>% 
    str_subset('(c|^y|[^glt]y|w|x|cz|tsz|[tgn]{2}y|ssz|zzs|[st]h|[^aáoóuúeé]{2}$|ll[^e])',negate = T)
  }
  res
}



# -- read -- #

ik = read_tsv('src/ik_verbs/ikes_pairs_webcorpus2.tsv')
vh = read_tsv('src/front_harmony/fh_pairs_webcorpus2.tsv')
ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- make words -- #

## vh
map_chr(1:10, ~ buildVH(vh_g))
  
# -- danger zone -- #





# -- ultra mega danger zone -- #
# edik
# [oő]dik
# lik
# zik
ik_g = ik %>% 
  distinct(stem) %>% 
  mutate(
    ik = str_replace(stem, '.$', 'ik'),
    category = case_when(
      str_detect(ik, 'edik') ~  'edik',
      str_detect(ik, '[oöóő]dik') ~ 'odik',
      str_detect(ik, 'lik') ~ 'lik',
      str_detect(ik, 'zik') ~ 'zik',
      T ~ 'other'
    )
  )

ik_g = ik %>% 
  distinct(stem) %>% 
  rowwise() %>% 
  mutate(
    `s t e m` = str_split(stem, '', simplify = T) %>% 
      paste(collapse = ' ')
  )

range(nchar(ik$stem))
ik_4g = ngram(ik_g$`s t e m`[nchar(ik_g$`s t e m`) >= 8], n = 4)
babble(ik_4g, genlen = round(runif(1, 4, 15)))
