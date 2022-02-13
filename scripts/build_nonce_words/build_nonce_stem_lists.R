## build nonce word prompts and targets from the three real word lists
# 1. build a set of plausible word-initial, word-final, and word-mid consonant clusters from lists of frequent verbs and nouns, separately
# 2. generate nonce words from the three sets
# 3. filter nonce words: no words too close to real words, no overlaps at beginning with real words, no words too close to one another

## previous file: build_real_words_list, but we don't use it much here
## next file: filter_nonce_stem_lists
## in: c
## out: noun and verb "grammars" (these are giant tables you can use as weighted ngram models). noun and verb stem lists.
## general notes
# there's a var called grammar_built. if it's F then you build grammar from c and build stems from grammar. if it's T you build stems from grammar.

## grammar building
# we extract beginnings, ends, and middles of consonant sequences in words, and then we exclude strings I don't like. not liking comes from a lot of trial and error and talking to people. we do this separately for verbs and nouns. we save the result to ns2 and vs2.

## stem building
# we specify the types of stems we want and then build them from noun/verb bits, respectively. vowel harmony done separately.

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# library(furrr)

# -- big red buttons -- #

# you've already built the grammars.
grammar_built = T

# -- read -- #

if (!grammar_built) {
  c = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')  
} else {
  ns2 = read_tsv('src/nonce_words/grammars/noun_grammar.tsv')
  vs2 = read_tsv('src/nonce_words/grammars/verb_grammar.tsv')
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
buildWords = function(grammar_type,nsyl,front){
  
  # this is shameful:
  if (grammar_type == 'noun') {
    dat = ns2
  } else if (grammar_type == 'verb'){
    dat = vs2
  } else {
    print('wrong grammar type, not verb or noun')
  }
  
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
    
  vowelHarmony(my_word,front = front)
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

	write_tsv(ns2, 'src/nonce_words/grammars/noun_grammar.tsv')
	write_tsv(vs2, 'src/nonce_words/grammars/verb_grammar.tsv')

} else {
  tom = readLines('notes/tomhutchinson.txt')
  print(tom)
  print('Tom Hutchinson shine upon you.')
}

# -- build stems -- #

# we want n/v stems of front/back vowels and 1-3 syl length. it was originally 4 syl but that's laughably long for verb stems (think balelülölődik)
stems = crossing(
  grammar_type = c('noun','verb'),
  nsyl = 1:3,
  front = c(T,F)
)

# for i = n stems per category, go through stem types in stems and build words with those parameters.
stems_list = as.list(NULL)

# I don't know how to map a map (outside map: go through 1:i. inside map: cols of stems go into buildWords.)
for (i in 1:2000){
  stems_list[[i]] = stems %>% 
    rowwise() %>% 
    mutate(
      word = pmap_chr(list(grammar_type,nsyl,front), ~ buildWords(grammar_type,nsyl,front))
    )
print(i)  
}

# combine everything.
stems_final = bind_rows(stems_list)

# -- write -- #

write_tsv(stems_final, 'src/nonce_words/stems/nonce_stems.tsv')
