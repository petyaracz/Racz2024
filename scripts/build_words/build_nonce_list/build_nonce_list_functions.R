####################################
# Build nonce word lists from scratch
# @raczpetya
####################################

# code logic:
# 1. draw 2-syl nouns and ik verbs from frequency list
# 2. break noun and verb forms into pieces and recombine those into new nonce words. do this separately for nouns, 1syl verbs, 2 syl verbs.
# 4. build final forms.
# 5. take final forms, filter them across similarity to existing words, similarity to each other.
# 6. gcm

## in: c, the webcorpus frequency list I made. xpostags for each var.
## out: nonce forms with endings for the psychopy script
## general notes
# the code is broken up into four parts. part one involves indexing a very large file and part three takes ages. which part is active is controlled by four vars (build1-4) that are on by default meaning that no parts are active and so running the code does nothing.
# the starting points are bisyllabic mixed V nouns (makes sense), and separately mono and bisyllabic verbs. words are taken apart into chunks that take phonotactics into account (word onset, vowel with all the C after it) and then combined freely. this results in stem V combinations that don't really exist (e+ö etc) so those are filtered out.

# -- header -- #

set.seed(1989)

library(tidyverse)
library(glue)
library(magrittr)

library(furrr)

# -- functions -- #

# take c, build a restricted list. the forms should be in our spellcheck list, they should have a vowel, they should not have quirkly letters, should have some threshold frequency, and be not overlong (to exclude most compounds)
buildList2 = function(dat){
  
  dat %>% 
    filter(
      hunspell,
      str_detect(lemma, '[aáeéiíoóöőuúüű]'),
      str_detect(lemma, '[qwx]', negate = T),
      lemma_freq > 1,
      nchar(lemma) < 10
    ) %>% 
    distinct(lemma,lemma_freq,corpus_size,xpostag)
}

# take string, transcribe to single digits digraphs or back to double digit ones, return string
transcribe = function(nonce_word,direction){
  
  case_when(
    direction == 'single' ~ nonce_word %>% 
      str_replace_all(., c('cs' = 'ç', 'sz' = 'ß', 'zs' = 'Ω', 'ty' = '†', 'gy' = '©', 'ny' = '¥', 'ly' = '¬')),
    direction == 'double' ~ nonce_word %>% 
      str_replace_all(., c('ç' = 'cs', 'ß' = 'sz', 'Ω' = 'zs', '†' = 'ty', '©' = 'gy', '¥' = 'ny', '¬' = 'ly')),
    T ~ 'wrong direction, either (to) "single" or "double"'
  )
}

# take string, count unique characters in word and yell T if one char occurs more than n times otherwise F
countUnique = function(word,n){
  max_count = word %>% 
    strsplit(split = '') %>% 
    unlist() %>% 
    table() %>% 
    as.double() %>% 
    max()
  max_count <= n
}

# take string, for a transcribed word, return boolean marking that vowels match in roundedness and frontness
checkVH = function(tr){
  my_vowels = tr %>% 
    str_extract_all(., '[aáeéiíoóöőuúüű]') %>% 
    unlist()
  
  v1 = my_vowels[1]
  v2 = my_vowels[2]
  
  roundness = 
    ( str_detect(v1, '[aáeéií]') & str_detect(v2, '[aáeéií]') ) |
    ( str_detect(v1, '[oóöőuúüű]') & str_detect(v2, '[oóöőuúüű]') )
  frontness = 
    ( str_detect(v1, '[aáoóuúií]') & str_detect(v2, '[aáoóuúií]') ) |
    ( str_detect(v1, '[eéiíöőüű]') & str_detect(v2, '[eéiíöőüű]') )
  
  roundness & frontness  
}

# take string, dictionary, distance var. match individual nonce word string against spelling dictionary. return T if it has an edit distance of distance var with any word in dictionary.  else return F.
matchReal = function(w,my_h, dist){
  dists = map(my_h, ~ stringdist::stringdist(., w, method = 'lv')) %>% 
    unlist()
  min(dists) >= dist
}

# take string, dictionary. for string, match beginnings and ends against spelling dictionary for overlaps. return T if no overlaps. else return F 
matchMargins = function(w,my_h){
  pass1 = !any(str_detect(w, glue('^{my_h}')))
  pass2 = !any(str_detect(w, glue('{my_h}$')))
  pass1 | pass2
}

# parallelised wrapper around the two matching functions
matchWrapper = function(d,h,h_margins,dist){
  d2 = d %>%
    rowwise() %>%
    mutate(
      keep_enough_distance = future_map_lgl(tr, ~ matchReal(., h, dist)),
      keep_no_overlap = future_map_lgl(tr, ~ matchMargins(., h_margins))
    )
  return(d2)
}

# take word vector. match words against themselves, within each set. drop word if any other word is closer than 1 distance. technically this removes both members of such a pair and is overkill. return filtered vector
matchNonce = function(words){
  formz = crossing(
    tr = words,
    match = words
  ) %>%
    filter(tr != match) %>%
    mutate(
      dist = stringdist::stringdist(tr, match, method = 'lv')
    )
  
  formz %<>%
    group_by(tr) %>%
    summarise(min_dist = min(dist))
  
  formz %>% 
    filter(min_dist > 1) %>%
    pull(tr)
}

# wrap similarity function and subsetter, take d, return d with new col
similarityWrapper = function(d){
  dw = d %>% 
    filter(keep_enough_distance,keep_no_overlap) %>% 
    pull(tr) %>% 
    matchNonce()
  
  d %>% 
    mutate(keep_self_diff = tr %in% dw)
}

# -- vars -- #

n_sample = 1024