## build nonce word prompts and targets from the three real word lists
# 1. build a set of plausible word-initial, word-final, and word-mid consonant clusters from lists of frequent verbs and nouns, separately
# 2. generate nonce words from the three sets
# 3. filter nonce words: no words too close to real words, no overlaps at beginning with real words, no words too close to one another

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

library(furrr)

# -- big red buttons -- #

# you've already built the grammars.
grammar_built = T

# -- read -- #

if (!grammar_built) {
  c = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')  
} else {
  ns2 = read_tsv('src/nonce_forms/noun_grammar.tsv')
  vs2 = read_tsv('src/nonce_forms/verb_grammar.tsv')
  ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
}


# -- functions -- #

## grammar building

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

# take pre-filtered noun/verb list, only first three syllables otherwise every word can be comically long, build sort of syllables, make list long
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
      # syl4 = lemma %>% str_extract(glue('(?<={syl1}{syl2}{syl3})[^aáeéiíoóöőuúüű]*[aáeéiíoóöőuúüű]')),
      word_mid1 = syl1 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'),
      word_mid2 = syl2 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'),
      # word_mid3 = syl3 %>% str_extract('[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'),
      ) %>% 
    select(lemma,word_start,word_end,word_mid1,word_mid2) %>% 
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
      !(syl_pos == 'word_mid' & str_detect(string, '(pzs|gk|rbl|nzp|lfp|ngn|szs|zsz|rsl|byt|ckz|dzk|ftv|khm|mb.|mph|^y$|yst|szd|tsz|gyz|nb|rgr|rsz|tny|nzn|lfl|jtj)')),
      !(syl_pos == 'word_end' & str_detect(string, '(s|ck|sb|fj|hm|nch|ndy|gn|ght|dys|yk|d.|m.|k.|p.|rj|rtz|tsz|lgy)'))
    )
  
}

## form building

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
  
  repeat_in_word = T
  
  while(repeat_in_word){
  
    my_word_start = sample(word_start,1)
    my_word_end = sample(word_end,1)
    my_mid1 = sample(word_mid,1)
    my_mid2 = sample(word_mid,1)
    my_mid3 = sample(word_mid,1)
    my_v1 = sample(v1,1)
    my_v2 = sample(v2,1)
    my_v3 = sample(v1,1)
    my_v4 = sample(v1,1)
      
    my_word = case_when(
        nsyl == 1 ~ glue('{my_word_start}{my_v1}{my_word_end}'),
        nsyl == 2 ~ glue('{my_word_start}{my_v1}{my_mid1}{my_v2}{my_word_end}'),
        nsyl == 3 ~ glue('{my_word_start}{my_v1}{my_mid1}{my_v2}{my_mid2}{my_v3}{my_word_end}'),
        nsyl == 4 ~ glue('{my_word_start}{my_v1}{my_mid1}{my_v2}{my_mid2}{my_v3}{my_mid3}{my_v4}{my_word_end}'),
        !(nsyl %in% 1:4) ~ 'We only do words between 1-4 syllables.'
      ) 
  # no repeats of same character. no long o u e + word-final CC.
    repeat_in_word = 
      str_detect(my_word, '(.+)\\1') |
      str_detect(my_word, '[óúé][^aáeéiíoóöőuúüű]+$')
  }
    
  my_word
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
  
  length = sample(1:3,1, prob = c(.45,.35,.2))
  front = sample(c(T,F),1)
  longv = sample(c(T,F),1)
  cat = sample(c('dik','zik','lik','sik','gyik','rik','kik'), 1, prob = c(.3,.3,.3,.025,.025,.025,.025))
  repeat_at_end = T
  
  while(repeat_at_end){
   
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
    
    # if word ends in lik, it can't be Vlik (no áramolik, only áramlik)
    my_vowel = case_when(
      cat %in% c('zik','rik') ~ sample(c(T,F),1),
      cat %in% c('lik') ~ F, 
      cat %in% c('dik','lik','sik','gyik','kik') ~ T
    )
    
    my_word = case_when(
      my_vowel ~ glue('{deriv}{v}{cat}'),
      !my_vowel ~ glue('{deriv}{cat}')
    )
    
    # no comical repeating characters like adódik, üllik, etc
    repeat_at_end = str_detect(my_word, '(d.dik$|llik$|z.zik$)') 
  }
  
  my_word
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
  
  length = sample(1:3,1, prob = c(.4,.4,.2))
  ep_line = sample_n(ep_letters,1)
  v = ep_line$v
  front = str_detect(v, '[eéiíöőüű]')
  rounded = str_detect(v, '[oóuúöőüű]')
  deriv = vs2 %>% 
    buildWords(length) %>% 
    vowelHarmony(front = front) %>% 
    str_replace('[^aáeéiíoóöőuúüű]+$','')
  
  # derivational endings do both rounding and front harmony. if people see something that looks like a derivational ending but doesn't work out with the linking vowel, they get confused and upset. so if something looks like a derivational ending it should act like it and do vowel harmony for rounding too.
  # this language will one day put me to the grave
  derivational_ending = ep_line$c3 %in% c('d','l','z')
  
  if (derivational_ending) {
    
    deriv2 = case_when(
      front & rounded ~ str_replace(deriv, '.$', 'ö'),
      !front & rounded ~ str_replace(deriv, '.$', 'o'),
      front & !rounded ~ str_replace(deriv, '.$', 'e'),
      !front & !rounded ~ str_replace(deriv, '.$', 'a')
    )
    
  } else {
    deriv2 = deriv
  }
    
    
  cvc = glue('{deriv2}{ep_line$c1}{v}{ep_line$c3}ik')
  cc = glue('{deriv2}{ep_line$c1}{ep_line$c2}ik')
  
  c(cvc,cc)
  
}

# -- build -- #

# build grammars, if you haven't already.
if ( !grammar_built ){

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

	write_tsv(ns2, 'src/nonce_forms/noun_grammar.tsv')
	write_tsv(vs2, 'src/nonce_forms/verb_grammar.tsv')

} else {
  readLines('notes/tomhutchinson.txt')
  print('Tom Hutchinson shine upon you.')
}

# build forms

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
  select(c1,c2,c3,v) %>% 
  ungroup()

# test

map_chr(1:10, ~ buildNoun(ns2))
map_chr(1:10, ~ buildIk(vs2))
map(1:10, ~ buildEp(vs2))

# actual build

plan(multisession, workers = 8)

## vh

nonce_vh = future_map_chr(1:2000, ~ buildNoun(ns2), .options = furrr_options(seed = 1337))

## ik

nonce_ik = future_map_chr(1:2000, ~ buildIk(vs2), .options = furrr_options(seed = 1337))

## epenthetic stems

nonce_ep = future_map(1:2000, ~ buildEp(vs2), .options = furrr_options(seed = 1337))

# (I'm sorry)

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
