####################################
# Build nonce word lists from scratch
# @raczpetya
####################################

# code logic:
# 1. draw 2-syl nouns and ik verbs from frequency list
# 2. break noun and verb forms into pieces and recombine those into new nonce words. do this separately for nouns, 1syl verbs, 2 syl verbs.
# 4. build final forms.
# 5. take final forms, filter them across similarity to existing words, similarity to each other.
# 6. some similarity thing

## in: c, the webcorpus frequency list I made. xpostags for each var.
## out: nonce forms with endings for the psychopy script
## general notes
# the code is broken up into four parts. part one involves indexing a very large file and part three takes ages.
# the starting points are bisyllabic mixed V nouns (makes sense), and separately mono and bisyllabic -CCik verbs that end in a derivational suffix (lik zik szik Vdik). words are taken apart into chunks that take phonotactics into account (word onset, vowel with all the C after it, etc) and then combined freely. this results in stem V combinations that don't really exist (e+ö etc) so those are filtered out.

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

# take word vector. iterate through words. if word i has a neighbour that's only 1 edit distance away, set boolean to F. otherwise set it to T. then exclude word i from comparison set and move on to word i+1. repeat. this means that if there's a pair of words that are 1 dist from one another, first one will be removed only. last word automatically wins. (no comparisons left). uses random seed.
matchNonce = function(words,my_dist){
  
  words2 = sample(words)
  passes = as.list(NULL)
  for (i in 1:length(words2)){
    
    if (i == length(words2)) break
    
    word = words2[i]
    words3 = words2[(i+1):length(words2)]
    
    dist = stringdist::stringdist(word, words3, method = 'lv') 
    passes[[i]] = min(dist) > my_dist 
    
  }
  passes = c(unlist(passes),T)
  tibble(
    tr = words2, #!!! how ugly
    keep_self_diff = passes
  )
  
}

# take set of words w/ variant comparisons, sum up freqs per stem for gcm for real forms, return sum
buildGCMtraining = function(dat){
  
  dat %>% 
    group_by(stem) %>% 
    summarise(
      tr = transcribe(stem, 'single'),
      freq_1 = sum(freq_1),
      freq_2 = sum(freq_2),
      log_odds = log( freq_1 / freq_2 )
    ) %>% 
    distinct()
}

# gcm! uses fur. expects training with word and category, word is string, category is string. expects test with word. uses future_map. returns test list with cat weights.
furGCM = function(training,test,var_s=0.3,var_p=1,distance_metric='lv'){
  
  training = training %>% droplevels 
  test = test %>% droplevels
  
  getTargetSimilarity = function(target,training){
    
    # we drop target from training in case we are cross-validating  
    training = training %>% 
      filter(
        word != target,
        !is.na(category)
      )
    
    training$target = target # this line is for tidier bookkeeping
    
    # for each row in training, calculate target ~ word-in-row distance and pairwise similarity  
    distances = training %>% 
      mutate(dist = stringdist::stringdist(word,target, method = distance_metric),
             pairwise.similarity = exp ( -dist / var_s )^var_p)
    
    # sum pairwise similarity for each category  
    category.distances = distances %>% 
      group_by(category) %>% 
      summarise(summed.pairwise.similarity = sum(pairwise.similarity))
    
    # get total similarity  
    total.similarity = distances %>% 
      summarise(sum(pairwise.similarity)) %>% 
      pull
    
    category.distances$total.similarity = total.similarity # this line is for tidier bookkeeping
    
    # get gcm weight of category  
    form.total.category.distances = category.distances %>% 
      mutate(gcm.weight = summed.pairwise.similarity / total.similarity) %>% 
      select(category, gcm.weight)
    
    # spread gcm weights. we end up with a line which is 'target w1, w2... wn'  
    ftdc.wide = form.total.category.distances %>% 
      spread(category, gcm.weight)
    
    return(ftdc.wide)
    
  }
  
  # for each target in test, get ftdc.wide using getTargetsimilarity and put into new cell, unnest. we won.  
  result = test %>% 
    mutate(similarities = furrr::future_map(word, ~ getTargetSimilarity(., training))) %>% 
    select(word, similarities) %>% 
    unnest(cols = c(similarities))
  
  return(result)
}

# -- vars -- #

n_sample = 1024