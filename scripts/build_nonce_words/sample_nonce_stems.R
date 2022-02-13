## assessing form similarity to existing forms, sampling forms down based on length, vowel, similarity

## previous file: filter_nonce_stem_lists
## next file: ...
## in: noun and verb stem list with filtering. variable real word lists made by build_real_word_list
## out: sampled nonce forms.
## general notes
# the three types of variation work in slightly different ways that completely mess up everything. the result is horribly complicated.
# for each variation, we set up two categories for variable forms. we use the generalised context model to predict nonce word membership in these two categories. this approximates similarity to real words, more or less
# this bins variation in real words, but it gives you one nice number at the end. which is helpful in sampling forms later.

## ik
# this is relatively straightforward: we create the 3sg form for nonce verbs and real verbs (eszik) and run the gcm on those.
# then we sample down the 10 similarity dociles x length x vowel. done. bingo.

## vh 
# same here. we create the nouns by drilling into the noun stems and changing vowels. this is bad for filtering, but hey. then we sample down the 10 similarity dociles x second vowel is e or é. yes good

## ep
# ho BOY
# we create ep verbs by taking a stem and a unique existing ep ending. for ending, think: cselekedik/cselekszik, so cvc:ked, cc:ksz. we use types of these, because we will sample this down anyway. we combine them with stems at random.
# we use the cvc form as the gcm training and target
# we move forms back and forth a LOT to be able to keep track of them.

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

library(furrr)
library(progress)
plan(multisession, workers = 8)

# -- functions -- #

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

# takes noun stem, make variable noun stem. 2syl only.
buildNoun = function(word){
  
  flag = T
  
  while (flag){
    
    vowels = str_extract_all(word, '[aáeéiíoóöőuúüű]', simplify = T)
    
    final_word = case_when(
      length(vowels) == 2 ~ word %>% 
        str_replace(vowels[1], sample(c('a','á','u','ú','o','ó'),1)) %>% 
        str_replace(vowels[2], sample(c('e','é'),1)),
      length(vowels) != 2 ~ "I want 2-syl words."
    )
    
    flag = str_detect(final_word, '[eé](?=[^aáeéiíoóöőuúüű]+$)', negate = T)
    
  }
  
  final_word # ¯\_(ツ)_/¯
  
}

# takes verb stem, picks CC and epenthetic vowel, and then builds CVC and CC form
buildEp = function(stem,ep_end){
  
  ep_line = sample_n(ep_end,1)
  v = ep_line$v
  front = str_detect(v, '[eéiíöőüű]')
  rounded = str_detect(v, '[oóuúöőüű]')
  deriv = stem %>% 
    vowelHarmony(front = front) %>% 
    str_replace('[^aáeéiíoóöőuúüű]+$','')
  
  # derivational endings do both rounding and front harmony. if people see something that looks like a derivational ending but doesn't work out with the linking vowel, they get confused and upset. so if something looks like a derivational ending it should act like it and do vowel harmony for rounding too.
  
  derivational_ending = ep_line$cvc_c2 %in% c('d','l','z')
  
  if (derivational_ending) {
    # this language will one day put me to the grave  
    deriv2 = case_when(
      front & rounded ~ str_replace(deriv, '.$', 'ö'),
      !front & rounded ~ str_replace(deriv, '.$', 'o'),
      front & !rounded ~ str_replace(deriv, '.$', 'e'),
      !front & !rounded ~ str_replace(deriv, '.$', 'a')
    )
    
  } else {
    deriv2 = deriv
  }
  
  cvc = glue('{deriv2}{ep_line$c1}{v}{ep_line$cvc_c2}ik')
  cc = glue('{deriv2}{ep_line$c1}{ep_line$cc_c2}ik')
  
  c(cvc,cc)
  
}

# takes web corpus 2 freq list from src/[variation] and returns gcm training categories which are first and third third of log odds of all forms for stem.
buildGCMtraining = function(dat){
  dat %>% 
    group_by(word) %>% 
    summarise(
      freq_1 = sum(freq_1),
      freq_2 = sum(freq_2),
      log_odds = log(freq_1 / freq_2)
    ) %>% 
    filter(freq_1 > 3 & freq_2 > 3) %>% 
    ungroup() %>% 
    mutate(
      ntile_log_odds = ntile(log_odds, 3),
      category = case_when(
        ntile_log_odds == 1 ~ 'low',
        ntile_log_odds == 3 ~ 'high'
      )
    )
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

# -- read -- #

nonce = read_tsv('src/nonce_words/stems/nonce_stems_filt_clean.tsv')
noun_comp = read_tsv('src/front_harmony/fh_pairs_webcorpus2.tsv')
ik_comp = read_tsv('src/ik_verbs/ikes_pairs_webcorpus2.tsv')
ep_comp = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- build -- #

## ik

# we pick the 1st half of verb stems, add ik.
ik = nonce %>% 
  filter(grammar_type == 'verb') %>% 
  rownames_to_column() %>% 
  filter(rowname < n()/2) %>% 
  select(word,front,nsyl) %>% 
  rowwise() %>% 
  mutate(verb = glue('{word}ik'))

# this is our test set.
ik_test = ik %>% 
  select(verb) %>% 
  rename('word' = verb) %>% 
  distinct() %>% 
  ungroup()

# our training set is variable verbs. we create a tidy training form that ends in ik.
ik_training = ik_comp %>% 
  mutate(word = str_replace(stem, '.$', 'ik')) %>% 
  buildGCMtraining() 

# a little autotune. s = .4 sounds about right
# ggplot(ik_training, aes(fct_reorder(word,log_odds),log_odds,colour = ntile_log_odds)) +
#   geom_point()
# ik_test = furGCM(ik_training,ik_training,var_s = .4, var_p = 1, distance_metric = 'lv')
# left_join(ik_training,ik_test) %>%
# summarise(cor = broom::tidy(cor.test(high,log_odds)))
# .3, .272  
# .4, .29
# .5 .3

# we get category weights for nonce forms
ik_out = furGCM(ik_training,ik_test,var_s = .4, var_p = 1, distance_metric = 'lv')
# hist(ik_out$high)

## vh

# we turn 2syl words into variable noun stems. this is not good for filtering (some stems might be turned into a real-looking word because of the vowel replacement) but we'll worry about that later, if ever
noun_test = nonce %>% 
  filter(grammar_type == 'noun')  %>% 
  select(word,front) %>% 
  rowwise() %>% 
  mutate(noun = buildNoun(word)) %>% 
  select(noun) %>% 
  rename('word' = noun) %>% 
  distinct() %>% 
  ungroup()

# we create a training set
noun_training = noun_comp %>% 
  mutate(word = stem) %>% 
  buildGCMtraining()  

# a little autotune. s = .4 sounds about right
# noun_test = furGCM(noun_training,noun_training,var_s = .4, var_p = 1, distance_metric = 'lv')
# left_join(noun_training,noun_test) %>% 
  # summarise(cor = broom::tidy(cor.test(high,log_odds)))
# .3, .384  
# .4, .4
# .6 .4

# we get category weights for nonce forms
noun_out = furGCM(noun_training,noun_test,var_s = .3, var_p = 1, distance_metric = 'lv')
# hist(noun_out$high)

## ep

# you need to extract the consonants from cc and cvc and the vowel from cvc. second c might be different based on stem. first c always the same.
ep_end = ep_comp %>% 
  filter(str_detect(form_1, 'dni$', negate = T)) %>% 
  select(stem,form_1,form_2) %>% 
  mutate(
    end_1 = str_extract(form_1, glue('(?<={stem}).*$')),
    end_2 = str_extract(form_2, glue('(?<={stem}).*$')),
    c1 = str_extract(stem, '(sz|ny|ly|zs|cs|gy|[^aáeéiíoóöőuúüű])$'),
    cvc_c2 = str_extract(end_2, '(?<=[aáeéiíoóöőuúüű])(sz|[^aáeéiíoóöőuúüű])'),
    cc_c2 = str_extract(end_1, '^(sz|[^aáeéiíoóöőuúüű])'),
    v = str_extract(end_2, '^[aáeéiíoóöőuúüű]')
  ) %>%
  distinct(c1,v,cvc_c2,cc_c2)
 
# you build ep forms by taking stems and turning them into ep verbs by randomly adding an ep ending.   
ep = nonce %>% 
  filter(grammar_type == 'verb') %>% 
  rownames_to_column() %>% 
  filter(rowname < n()/2) %>% 
  select(word,front,nsyl) %>% 
  rowwise() %>% 
  mutate(
    verb = list(buildEp(word,ep_end)),
    cvc = verb[[1]],
    cc = verb[[2]]
  )

ep_test = ep %>% 
  select(cvc) %>% 
  rename('word' = cvc) %>% 
  distinct() %>% 
  ungroup()

# ep_comp %>% 
#   count(xpostag)

ep_training = ep_comp %>%
  rename('word' = stem) %>% 
  buildGCMtraining()

ep_training = ep_comp %>%
  rename('word' = stem) %>% 
  filter(xpostag == '[/V][Prs.NDef.3Pl]') %>% 
  mutate(word2 = str_replace(form_2, 'n.k$', 'ik')) %>% 
  select(word,word2) %>% 
  inner_join(ep_training,., by = 'word') %>% 
  select(-word) %>% 
  rename('word' = word2)

# ggplot(ep_training, aes(fct_reorder(word,log_odds),log_odds,colour = ntile_log_odds)) +
  # geom_point()
# ep_test = furGCM(ep_training,ep_training,var_s = .4, var_p = 1, distance_metric = 'lv')
# left_join(ep_training,ep_test) %>%
# summarise(cor = broom::tidy(cor.test(high,log_odds)))
# .3, .39  
# .4, .4
# .5 .5
ep_out = furGCM(ep_training,ep_test,var_s = .4, var_p = 1, distance_metric = 'lv')

# -- merge -- #

ik %<>% 
  select(-word) %>% 
  rename('word' = verb) %>% 
  left_join(ik_out, by = 'word') %>% 
  select(-low) %>% 
  rename('gcm_weight' = high) %>% 
  mutate(gcm_decile = ntile(gcm_weight,10))

vh = noun_out %>% 
  ungroup() %>% 
  mutate(vowel = str_extract(word,'[eé]')) %>% 
  select(-low) %>% 
  rename('gcm_weight' = high) %>% 
  mutate(gcm_decile = ntile(gcm_weight,10))

ep = ep_out %>% 
  rename('cvc' = word) %>% 
  inner_join(ep) %>% 
  select(-front) %>% 
  mutate(
    nsyl = str_count(cvc, '[aáeéiíoóöőuúüű]')-2,
    ending = str_extract(cvc, '..ik$'),
    linking_vowel = str_extract(ending, '.'),
    suffix = str_extract(ending, '.ik')
  ) %>% 
  select(-low) %>% 
  rename('gcm_weight' = high) %>% 
  mutate(gcm_decile = ntile(gcm_weight,10)) %>% 
  filter(str_detect(linking_vowel, '[uü]', negate = T))

# -- sample -- #

# something weird with ik

vh %>% 
  count(vowel,gcm_decile) %>% 
  pivot_wider(names_from = gcm_decile, values_from = n)

vh2 = vh %>% 
  group_by(vowel,gcm_decile) %>% 
  sample_n(20, replace = T) %>% 
  distinct()

ep %>% 
  filter(str_detect(linking_vowel, '[uü]', negate = T)) %>% 
  count(suffix,linking_vowel) %>% 
  pivot_wider(names_from = linking_vowel, values_from = n)

# something weird here.

# -- write -- #

write_tsv(vh2, 'src/nonce_words/forms/vh_prompts.tsv')
