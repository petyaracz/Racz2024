## generate nonce words from the three sets

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

library(furrr)

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
  
  v1 = str_split('aáeéiíoóöőuúüű', '', simplify = T)
  v2 = str_split('aeiíoöuü', '', simplify = T)
  
  my_word_start = sample(word_start,1)
  my_word_end = sample(word_end,1)
  my_mid1 = sample(word_mid,1)
  my_mid2 = sample(word_mid,1)
  my_mid3 = sample(word_mid,1)
  my_v1 = sample(v1,1)
  my_v2 = sample(v2,1)
  my_v3 = sample(v1,1)
  my_v4 = sample(v1,1)
  
  case_when(
    nsyl == 1 ~ glue('{my_word_start}{my_v1}{my_word_end}'),
    nsyl == 2 ~ glue('{my_word_start}{my_v1}{my_mid1}{my_v2}{my_word_end}'),
    nsyl == 3 ~ glue('{my_word_start}{my_v1}{my_mid1}{my_v2}{my_mid2}{my_v3}{my_word_end}'),
    nsyl == 4 ~ glue('{my_word_start}{my_v1}{my_mid1}{my_v2}{my_mid2}{my_v3}{my_mid3}{my_v4}{my_word_end}'),
    !(nsyl %in% 1:4) ~ 'We only do words between 1-4 syllables.'
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

# take verb syllabary, toss a coin on how long the stem is, whether it's a front verb, and whether it's a long suffix (ódik vs odik), glue verb together. random!
buildIk = function(vs2){
  
  length = sample(1:4,1, prob = c(.4,.4,.15,.05))
  front = sample(c(T,F),1)
  cat = sample(c('dik','zik','lik'),1)
  longv = sample(c(T,F),1)
  deriv = vs2 %>% 
    buildWords(length) %>% 
    vowelHarmony(front = front)
  v = 
    case_when(
      str_detect(deriv, '[öőüű](?=[^aáeéiíoóöőuúüű]+$)') & !longv ~ 'ö',
      str_detect(deriv, '[öőüű](?=[^aáeéiíoóöőuúüű]+$)') & longv ~ 'ő',
      str_detect(deriv, '[eéií](?=[^aáeéiíoóöőuúüű]+$)') ~ 'e',
      str_detect(deriv, '[aáoóuú](?=[^aáeéiíoóöőuúüű]+$)') & !longv ~ 'o',
      str_detect(deriv, '[aáoóuú](?=[^aáeéiíoóöőuúüű]+$)') & longv ~ 'ó'
    )
  glue('{deriv}{v}{cat}')
}

# takes noun syllabary, make variable noun stem. 2syl only. uses random seed.
buildNoun = function(ns2){
  
  flag = T
  
  while (flag){
  
  nonce_word = buildWords(ns2,2)
  
  vowels = str_extract_all(nonce_word, '[aáeéiíoóöőuúüű]', simplify = T)
  
  final_word = case_when(
    length(vowels) == 2 ~ nonce_word %>% 
      str_replace(vowels[1], sample(c('a','á','u','ú','o','ó'),1)) %>% 
      str_replace(vowels[2], sample(c('e','é'),1)),
    length(vowels) != 2 ~ "I want 2-syl words."
  )
  
  flag = str_detect(final_word, '[eé](?=[^aáeéiíoóöőuúüű]+$)', negate = T)
  
  }
  
  final_word # ¯\_(ツ)_/¯

}

# takes verb syllabary, rolls for stem length, stem front/backness, whether verb is gonna be cvc or cc, picks epenthetic vowel, and then glues everything together
buildEp = function(vs2){
  
  length = sample(1:4,1, prob = c(.4,.4,.15,.05))
  front = sample(c(T,F),1)
  ep_line = sample_n(ep_letters,1)
  deriv = vs2 %>% 
    buildWords(length) %>% 
    vowelHarmony(front = front) %>% 
    str_replace('[^aáeéiíoóöőuúüű]+$','')
  v = case_when(
      front == F ~ sample(str_split('aou', '', simplify = T),1),
      front == T ~ sample(str_split('eiöü', '', simplify = T),1)
    )
    cvc = glue('{deriv}{ep_line$c1}{v}{ep_line$c3}ik')
    cc = glue('{deriv}{ep_line$c1}{ep_line$c2}ik')
  
    c(cvc,cc)
}

# -- wrangling -- #

ep_letters = ep %>% 
  select(form_1,form_2,stem) %>% 
  rowwise() %>% 
  mutate(
    c1 = str_extract(stem, '(sz|zs|ty|gy|ny|[rtpsdfghjklzcvbnm])$'),
    c2 = str_extract(form_1, glue('(?<={stem})(sz|zs|ty|gy|ny|ly|[rtpsdfghjklzcvbnm])')),
    v = str_extract(form_2, glue('(?<={stem})[aáeéiíoóöőuúüű]')),
    c3 = str_extract(form_2, glue('(?<={stem}{v})(sz|zs|ty|gy|ny|ly|[rtpsdfghjklzcvbnm])'))
  ) %>%
  filter(!is.na(c1)) %>% 
  distinct(c1,c2,c3) %>% 
  ungroup()

# -- build -- #

plan(multisession, workers = 8)

## vh

nonce_vh = future_map_chr(1:10^3, ~ buildNoun(ns2), .options = furrr_options(seed = 1337))

## ik

nonce_ik = future_map_chr(1:10^3, ~ buildIk(vs2), .options = furrr_options(seed = 1337))

## epenthetic stems

nonce_ep = future_map(1:10^3, ~ buildEp(vs2), .options = furrr_options(seed = 1337))

# -- I'm sorry -- #

nonce_ik %<>% tibble()
nonce_vh %<>% tibble()
nonce_ep %<>% tibble()

names(nonce_ik) = 'word'
names(nonce_vh) = 'word'
names(nonce_ep) = 'words'

nonce_ep %<>% 
  mutate(
    cvc = map_chr(words, ~ nth(.,1)),
    cc = map_chr(words, ~ nth(.,2))
  ) %>% 
  pivot_longer(-words, names_to = 'type', values_to = 'word') %>% 
  select(-words)
  
# -- write -- #

nonce_vh %>% 
  write_tsv('nonce_words/raw/nonce_vh.txt')

nonce_ik %>% 
  write_tsv('nonce_words/raw/nonce_ik.txt')

nonce_ep %>% 
  write_tsv('nonce_words/raw/nonce_ep.txt')
