## build a set of plausible word-initial, word-final, and word-mid consonant clusters from lists of frequent verbs and nouns, separately

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# -- read -- #

c = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')

# -- functions -- #

# take c, build list. we want to build a syllabary. we pick a separate noun and verb list. for each, the forms should be in our spellcheck list, they should have a vowel, they should not have quirkly letters, should have some threshold frequency, and be not overlong (to exclude most compounds)

buildList = function(dat,my_xpostag){
  
  dat %>% 
    filter(
      xpostag == my_xpostag,
      hunspell,
      str_detect(lemma, '[aáeéiíoóöőuúüű]'),
      str_detect(lemma, '[qwx]', negate = T),
      lemma_freq > 1000,
      nchar(lemma) < 10
    ) %>% 
    distinct(lemma,lemma_freq,corpus_size)
}

# take pre-filtered noun/verb list, build sort of syllables, make list long
createSyllables = function(dat){
  
  dat2 = dat %>% 
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
    select(lemma,word_start,word_end,word_mid1,word_mid2,word_mid3,word_mid4) %>% 
    pivot_longer(-lemma,names_to = 'syl_pos', values_to = 'string') %>% 
    filter(!is.na(string)) %>% 
    mutate(
      syl_pos = str_replace(syl_pos, '(?<=word_mid)[1234]', '')
    ) %>% 
    select(-lemma)
}

# takes sort of syl list, uniques clusters, removes odd ones
stripClusters = function(dat){
  dat %>% 
    filter(
      str_detect(string, '(.)\\1{1,}', negate = T),
      str_detect(string, '(c|h|ly)', negate = T),
      nchar(string) < 4,
      !(syl_pos == 'word_start' & str_detect(string, '(fj|y|pn|psz|byt|kc|khm|mb|sh|szc|th)')), 
      !(syl_pos == 'word_mid' & str_detect(string, '(pzs|gk|rbl|nzp|lfp|ngn|szs|zsz|rsl|byt|ckz|dzk|ftv|khm|mb.|mph|^y$|yst)')),
      !(syl_pos == 'word_end' & str_detect(string, '(ck|sb|fj|hm|nch|ndy|gn|ght|dys|yk|d.|m.|k.|p.|rj|rtz|tsz|lgy)'))
    )
  
}


# -- build stems -- #

nouns = c %>% 
  buildList('[/N][Nom]')

verbs = c %>% 
  buildList('[/V][Prs.NDef.3Sg]')
  
# build syllabary

ns = createSyllables(nouns)
vs = createSyllables(verbs)

# simplify, tidy

ns2 = ns %>% 
  stripClusters()

vs2 = vs %>% 
  stripClusters() 

# -- write -- #

write_tsv(ns2, 'src/nonce_forms/noun_grammar.tsv')
write_tsv(vs2, 'src/nonce_forms/verb_grammar.tsv')
