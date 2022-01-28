## generate nonce words from the three sets

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

library(ngram)

# -- read -- #

c = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')
ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- functions -- #

# take pre-filtered noun/verb list, build sort of syllables, make list long
createSyllables = function(dat){
  
  dat %>% 
    rowwise() %>% 
    mutate(
      nsyl = lemma %>% str_count('[aáeéiíoóöőuúüű]'),
      word_start = lemma %>% str_extract('^[^aáeéiíoóöőuúüű]+'),
      word_end = lemma %>% str_extract('[^aáeéiíoóöőuúüű]+$'),
      syl1 = lemma %>% str_extract('^[^aáeéiíoóöőuúüű]*[aáeéiíoóöőuúüű]'),
      syl2 = lemma %>% str_extract(glue('(?<={syl1})[^aáeéiíoóöőuúüű]*[aáeéiíoóöőuúüű]')),
      syl3 = lemma %>% str_extract(glue('(?<={syl1}{syl2})[^aáeéiíoóöőuúüű]*[aáeéiíoóöőuúüű]')),
      syl4 = lemma %>% str_extract(glue('(?<={syl1}{syl2}{syl3})[^aáeéiíoóöőuúüű]*[aáeéiíoóöőuúüű]')),
      word_mid1 = syl1 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'),
      word_mid2 = syl2 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'),
      word_mid3 = syl3 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'),
      word_mid4 = syl4 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])')
    ) %>% 
    pivot_longer(-c(lemma,nsyl), names_to = 'syl_pos', values_to = 'string') %>% 
    filter(!(syl_pos %in% c('syl1','syl2','syl3','syl4')),!is.na(string)) %>% 
    mutate(
      syl_pos = str_replace(syl_pos, '(?<=word_mid)[1234]', '')
    )
}

# takes the syl treasury output by createsyllables and a syl length and outputs a nonce word of that syl length. random! relies on a glob env seed.
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
    !(nsyl %in% 1:4) ~ "We don't do that round here."
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

# takes output of vowelHarmony / buildWords, turns into variable noun stem. 2syl only. uses random seed.
hammerIntoNoun = function(nonce_word){
  
  vowels = str_extract_all(nonce_word, '[aáeéiíoóöőuúüű]', simplify = T)
  
  case_when(
    length(vowels) == 2 ~ nonce_word %>% 
      str_replace(vowels[1], sample(c('a','á','u','ú','o','ó'),1)) %>% 
      str_replace(vowels[2], sample(c('e','é'),1)),
    length(vowels) != 2 ~ "We don't do that round here."
  )
  
}

# -- build stems -- #

nouns = c %>% 
  filter(
    xpostag == '[/N][Nom]',
    hunspell,
    str_detect(lemma, '[aáeéiíoóöőuúüű]'),
    str_detect(lemma, '[qwx]', negate = T),
    lemma_freq > 3
    ) %>% 
  distinct(lemma)

verbs = c %>% 
  filter(
    xpostag == '[/V][Prs.NDef.3Sg]',
    hunspell,
    str_detect(lemma, '[aáeéiíoóöőuúüű]'),
    str_detect(lemma, '[qwx]', negate = T),
    lemma_freq > 3
    ) %>% 
  distinct(lemma)

# rm(c)

## character-level
# noun_character_grammar = nouns %>%
#   rowwise() %>%
#   mutate(
#     lemma2 = lemma %>% 
#       str_replace_all('sz','S') %>% 
#       str_replace_all('zs','Z') %>% 
#       str_replace_all('ty','T') %>% 
#       str_replace_all('gy','G') %>% 
#       str_replace_all('ny','N') %>% 
#       str_split('', simplify = T) %>%
#       paste(collapse = ' ')
#   ) %>%
#   filter(nchar(lemma2) >= 8) %>% 
#   pull(lemma2) %>%
#   ngram(n = 3)
# 
# babble(noun_character_grammar, genlen = round(runif(1, 4, 10))) %>% 
#   str_replace_all(' ', '') %>% 
#   str_replace_all('S','sz') %>% 
#   str_replace_all('Z','zs') %>% 
#   str_replace_all('T','ty') %>% 
#   str_replace_all('G','gy') %>% 
#   str_replace_all('N','ny')
## No.

# syllable-level

ns = createSyllables(nouns)
vs = createSyllables(verbs)

# -- danger zone -- #

## vh: easy, two-syl only, replace 1st with back vowel, 2nd with e/é

map_chr(1:10, ~ ns %>% 
          buildWords(ns) %>% 
          hammerIntoNoun()
        )
## ik

# front
map_chr(1:10, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = T) %>% 
          glue("{sample(c('ed','öd','őd','l','z'),1)}ik")
        )

# back
map_chr(1:10, ~ vs %>% 
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
map_chr(1:10, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = T) %>% 
          str_replace('[^aáeéiíoóöőuúüű]+$','') %>% 
          glue('{sample(c1c2,1)}ik')
        )
# back
map_chr(1:10, ~ vs %>% 
          buildWords(2) %>% 
          vowelHarmony(front = F) %>% 
          str_replace('[^aáeéiíoóöőuúüű]+$','') %>% 
          glue('{sample(c1c2,1)}ik')
)

## to do

# epenthetic stems, target forms: watch out for z/sz etc pairings. maybe pair up cc and cvc for each verb and use those?

# in general: channel output in some way that isn't dumped into the console. shiny app with button?
