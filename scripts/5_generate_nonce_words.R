## generate nonce words from the three sets

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# -- read -- #

ns2 = read_tsv('src/nonce_forms/noun_grammar.tsv')
vs2 = read_tsv('src/nonce_forms/verb_grammar.tsv')
ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- functions -- #

# takes the syl treasury output by createsyllables in 4_build_nonce_word_grammar and a syl length and outputs a nonce word of that syl length. random! relies on a glob env seed.
buildWords = function(dat,nsyl){
  
  word_start = dat %>% 
    filter(syl_pos == 'word_start') %>% 
    pull(string)
  
  word_mid = dat %>% 
    filter(syl_pos == 'word_mid') %>% 
    pull(string)
  
  word_end = dat %>% 
    filter(syl_pos == 'word_end') %>% 
    pull(string)
  
  v = str_split('aáeéiíoóöőuúüű', '', simplify = T)
  
  case_when(
    nsyl == 1 ~ glue('{sample(word_start,1)}{sample(v,1)}{sample(word_end,1)}'),
    nsyl == 2 ~ glue('{sample(word_start,1)}{sample(v,1)}{sample(word_mid,1)}{sample(v,1)}{sample(word_end,1)}'),
    nsyl == 3 ~ glue('{sample(word_start,1)}{sample(v,1)}{sample(word_mid,1)}{sample(v,1)}{sample(word_mid,1)}{sample(v,1)}{sample(word_end,1)}'),
    nsyl == 4 ~ glue('{sample(word_start,1)}{sample(v,1)}{sample(word_mid,1)}{sample(v,1)}{sample(word_mid,1)}{sample(v,1)}{sample(word_mid,1)}{sample(v,1)}{sample(word_end,1)}'),
    !(nsyl %in% 1:4) ~ "Only 1-4 syllable length works."
  )
  
}

# takes output of vowelHarmony / buildWords, turns into variable noun stem. 2syl only. uses random seed.
hammerIntoNoun = function(nonce_word){
  
  vowels = str_extract_all(nonce_word, '[aáeéiíoóöőuúüű]', simplify = T)
  
  case_when(
    length(vowels) == 2 ~ nonce_word %>% 
      str_replace(vowels[1], sample(c('a','á','u','ú','o','ó'),1)) %>% 
      str_replace(vowels[2], sample(c('e','é'),1)),
    length(vowels) != 2 ~ "I want 2-syl words."
  )
  
}

# take a string and a front/not front specification and change the vowels therein
vowelHarmony = function(nonce_word,front){
  
  case_when(
    front ~ nonce_word %>% 
      str_replace_all('a', 'e') %>% 
      str_replace_all('á', 'é') %>% 
      str_replace_all('o', 'ö') %>% 
      str_replace_all('ó', 'ő') %>% 
      str_replace_all('u', 'ü') %>% 
      str_replace_all('ú', 'ű'),
    !front ~ nonce_word %>% 
      str_replace_all('e', 'a') %>% 
      str_replace_all('é', 'á') %>% 
      str_replace_all('ö', 'o') %>% 
      str_replace_all('ő', 'ó') %>% 
      str_replace_all('ü', 'u') %>% 
      str_replace_all('ű', 'ú')
  )
}

# -- build -- #

## vh: easy, two-syl only, replace 1st with back vowel, 2nd with e/é

nonce_vh = map_chr(1:10^4, ~ ns2 %>% 
          buildWords(2) %>% 
          hammerIntoNoun()
)

## ik

# front
nonce_ik1 = map_chr(1:10^4, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = T) %>% 
          glue("{sample(c('ed','öd','őd','l','z'),1)}ik")
)

# back
nonce_ik2 = map_chr(1:10^4, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = F) %>% 
          glue("{sample(c('od','l','z'),1)}ik")
)

## epenthetic stems

c1c2 = ep %>% 
  select(form_1,stem) %>% 
  rowwise() %>% 
  mutate(
    c1 = str_extract(stem, '(sz|zs|ty|gy|ny|[rtpsdfghjklzcvbnm])$'),
    c2 = str_extract(form_1, glue('(?<={stem})(sz|zs|ty|gy|ny|ly|[rtpsdfghjklzcvbnm])')),
    c1c2 = glue('{c1}{c2}')
  ) %>%
  distinct(c1c2) %>% 
  pull(c1c2)

# front
nonce_ep1 = map_chr(1:10^4, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = T) %>% 
          str_replace('[^aáeéiíoóöőuúüű]+$','') %>% 
          glue('{sample(c1c2,1)}ik')
)

# back
nonce_ep2 = map_chr(1:10^4, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = F) %>% 
          str_replace('[^aáeéiíoóöőuúüű]+$','') %>% 
          glue('{sample(c1c2,1)}ik')
)

nonce_ik = c(nonce_ik1,nonce_ik2)
nonce_ep = c(nonce_ep1,nonce_ep2)

# -- write -- #

nonce_vh %>% 
  tibble() %>% 
  write_tsv('nonce_words/nonce_vh.txt')

nonce_ik %>% 
  tibble() %>% 
  write_tsv('nonce_words/nonce_ik.txt')

nonce_ep %>% 
  tibble() %>% 
  write_tsv('nonce_words/nonce_ep.txt')
