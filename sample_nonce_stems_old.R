######################################################
# stimulus builder: 5. sample stems and build forms
# @raczpetya
######################################################


## assessing form similarity to existing forms, sampling forms down based on length, vowel, similarity

## previous file: filter_nonce_stem_lists
## next file: ...
## in: noun and verb stem list with filtering. variable real word lists made by build_real_word_list
## out: sampled nonce forms.
## general notes
# words with stems more than 2 syl long look laughable, so I dropped them.
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
# we use the cc form as the gcm training and target because some verbs have no cvc form. it matters a bit for edit distance but probably not too much. (based on my paper on these verbs)
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

# transcribe to single digits digraphs or back to double digit ones
transcribe = function(nonce_word,direction){
  
  case_when(
    direction == 'single' ~ nonce_word %>% 
      str_replace_all(., c('cs' = 'ç', 'sz' = 'ß', 'zs' = 'Ω', 'ty' = '†', 'gy' = '©', 'ny' = '¥')),
    direction == 'double' ~ nonce_word %>% 
      str_replace_all(., c('ç' = 'cs', 'ß' = 'sz', 'Ω' = 'zs', '†' = 'ty', '©' = 'gy', '¥' = 'ny')),
    T ~ 'wrong direction, either (to) "single" or "double"'
  )
}

# take a string and a front/not front specification and change the vowels therein
vowelHarmony = function(nonce_word,front){
  
  case_when(
    front ~ nonce_word %>% 
      str_replace_all(., c('a' = 'e', 'á' = 'é', 'o' = 'ö', 'ó' = 'ő', 'u' = 'ü', 'ú' = 'ű')),
    !front ~ nonce_word %>% 
      str_replace_all(., c('e' = 'a','é' = 'á', 'ö' = 'o', 'ő' = 'ó', 'ü' = 'u', 'ű' = 'ú'))
  )
}

# takes noun stem, make variable noun stem. 2syl only.
buildNoun = function(word){
  
  flag = T
  
  while (flag){
    
    vowels = str_extract_all(word, '[aáeéiíoóöőuúüű]', simplify = T)
    
    final_word = case_when(
      length(vowels) == 2 ~ word %>% 
        str_replace(vowels[2], sample(c('e','é'),1)),
      length(vowels) != 2 ~ "I want 2-syl words."
    )
    
    flag = str_detect(final_word, 'é(?=[^aáeéiíoóöőuúüű]{2,}$)')
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

nonce = read_tsv('resource/nonce_words/stems/nonce_stems_filt_clean.tsv')
noun_comp = read_tsv('resource/front_harmony/fh_pairs_webcorpus2.tsv')
ik_comp = read_tsv('resource/ik_verbs/ikes_pairs_webcorpus2.tsv')
ep_comp = read_tsv('resource/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- wrangle -- #

nonce %<>% 
  sample_n(n())

ik_comp %<>%
  mutate(stem_tr = transcribe(stem, 'single'))

noun_comp %<>%
  mutate(stem_tr = transcribe(stem, 'single'))

ep_comp %<>%
  mutate(
    stem_tr = transcribe(stem, 'single'),
    form_1_tr = transcribe(form_1, 'single'),
    form_2_tr = transcribe(form_2, 'single'),
    )

# -- build -- #

## ik

# we pick the 1st half of verb stems, add ik.
ik = nonce %>% 
  rename('stem' = word) %>% 
  filter(
    grammar_type == 'verb',
    nsyl < 3
         ) %>% 
  rownames_to_column() %>%
  mutate(rowname = as.double(rowname)) %>% 
  filter(
    rowname < n()/2
    ) %>% 
  select(stem,front,nsyl) %>% 
  rowwise() %>% 
  mutate('word' = glue('{stem}ik'))

# this is our test set.
ik_test = ik %>% 
  select(word) %>% 
  distinct() %>% 
  ungroup()

# our training set is variable verbs. we create a tidy training form that ends in ik.
ik_training = ik_comp %>% 
  mutate(word = str_replace(stem_tr, '.$', 'ik')) %>% 
  buildGCMtraining() 

# a little autotune. s = .5 sounds about right
# ggplot(ik_training, aes(fct_reorder(word,log_odds),log_odds,colour = ntile_log_odds)) +
# geom_point()
# ik_test = furGCM(ik_training,ik_training,var_s = .5, var_p = 1, distance_metric = 'lv')
# left_join(ik_training,ik_test) %>%
# summarise(cor = broom::tidy(cor.test(high,log_odds)))
# .3, .327  
# .4, .349
# .5 .367
# .6 .38
# .7 .38

# we get category weights for nonce forms
ik_out = furGCM(ik_training,ik_test,var_s = .5, var_p = 1, distance_metric = 'lv')
# hist(ik_out$high)

## vh

# we turn 2syl back stems into variable noun stems. this is not good for filtering (some stems might be turned into a real-looking word because of the replacement of the 2nd vowel) but we'll worry about that later, if ever
noun_test = nonce %>% 
  rename('stem' = word) %>% 
  filter(grammar_type == 'noun')  %>% 
  select(stem,front) %>% 
  rowwise() %>% 
  mutate(word = buildNoun(stem)) %>% 
  select(word) %>% 
  distinct() %>% 
  ungroup()

# we create a training set
noun_training = noun_comp %>% 
  mutate(word = stem_tr) %>% 
  buildGCMtraining()  

# a little autotune. s = .5 sounds about right
# noun_test = furGCM(noun_training,noun_training,var_s = .5, var_p = 1, distance_metric = 'lv')
# left_join(noun_training,noun_test) %>%
  # summarise(cor = broom::tidy(cor.test(high,log_odds)))
# .3, .396  
# .4, .427
# .5 .446
# .6 .456

# we get category weights for nonce forms
noun_out = furGCM(noun_training,noun_test,var_s = .5, var_p = 1, distance_metric = 'lv')
# hist(noun_out$high)

## ep

# you need to extract the consonants from cc and cvc and the vowel from cvc. second c might be different based on stem. first c always the same. using ndef3pl as most freq v-init xpostag (so we get alternations)
ep_end = ep_comp %>% 
  filter(
    xpostag == '[/V][Prs.NDef.3Pl]', #those where d/sz alteration happens, also most freq xpost
    str_detect(form_1_tr, 'dni$', negate = T)
    ) %>%
  select(stem_tr,form_1_tr,form_2_tr) %>% 
  mutate(
    end_1 = str_extract(form_1_tr, glue('(?<={stem_tr}).*$')),
    end_2 = str_extract(form_2_tr, glue('(?<={stem_tr}).*$')),
    c1 = str_extract(stem_tr, '[^aáeéiíoóöőuúüű]$'),
    cvc_c2 = str_extract(end_2, '(?<=[aáeéiíoóöőuúüű])[^aáeéiíoóöőuúüű]'),
    cc_c2 = str_extract(end_1, '^[^aáeéiíoóöőuúüű]'),
    v = str_extract(end_2, '^[aáeéiíoóöőuúüű]')
  ) %>% 
  select(c1,v,cvc_c2,cc_c2)
 
# you build ep forms by taking stems and turning them into ep verbs by randomly adding an ep ending. Vlik forms don't exist, but we keep them for reference for now.   
ep = nonce %>% 
  rename('stem' = word) %>% 
  filter(
    grammar_type == 'verb',
    nsyl < 3
    ) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.double(rowname)) %>% 
  filter(
    rowname >= n()/2
    ) %>% 
  select(stem,front,nsyl) %>% 
  rowwise() %>% 
  mutate(
    word = list(buildEp(stem,ep_end)),
    cvc = word[[1]],
    cc = word[[2]]
  )

# some forms were made several times, also we don't need stem etc.
ep %<>% 
  ungroup() %>% 
  select(-stem,-front,-nsyl) %>% 
  distinct()

ep_test = ep %>% 
  select(cc) %>% 
  rename('word' = cc) %>% 
  distinct() %>% 
  ungroup()

# ep_comp %>% 
#   count(xpostag)

# we grab cc forms for stems
ep_word_cc = ep_comp %>%
  filter(xpostag == '[/V][Prs.NDef.3Pl]') %>% 
  mutate(word = str_replace(form_1_tr, '.n.k$', 'ik')) %>% 
  select(word,stem_tr) 

# we build training grouping under "word" (= cc form)
ep_training = ep_comp %>% 
  left_join(ep_word_cc, by = "stem_tr") %>%
  buildGCMtraining()

ep_out = furGCM(ep_training,ep_test,var_s = .5, var_p = 1, distance_metric = 'lv')
# hist(ep_out$high)

# -- merge -- #

ik2 = ik %>% 
  ungroup() %>% 
  inner_join(ik_out, by = 'word') %>% 
  select(-low) %>% 
  rename('gcm_weight' = high) %>% 
  mutate(
    word = transcribe(word,'double'),
    gcm_decile = ntile(gcm_weight,10)
    )

vh2 = noun_out %>% # vh2 called vh2 for naming consistency, even though no vh
  ungroup() %>% 
  mutate(vowel = str_extract(word,'[eé]')) %>% 
  select(-low) %>% 
  rename('gcm_weight' = high) %>% 
  mutate(gcm_decile = ntile(gcm_weight,10))

ep2 = ep_out %>% 
  rename('cc' = word) %>% 
  inner_join(ep, by = "cc") %>% 
  mutate(
    nsyl = str_count(cvc, '[aáeéiíoóöőuúüű]')-2,
    ending1 = str_extract(cvc, '[aáeéiíoóöőuúüű](sz|ty|gy|ny|zs|[^aáeéiíoóöőuúüű])ik$'),
    suffix2 = str_extract(cc, '(sz|ty|gy|ny|zs|[^aáeéiíoóöőuúüű])ik$'),
    linking_vowel = str_extract(ending1, '.'),
    suffix1 = str_extract(ending1, '.ik')
  ) %>% 
  select(-low) %>% 
  rename('gcm_weight' = high) %>% 
  mutate(gcm_decile = ntile(gcm_weight,10)) 

# -- sample -- #

ik2 %>% 
  count(front,nsyl,gcm_decile) %>% 
  pivot_wider(names_from = gcm_decile, values_from = n)

ik2 %>% 
  count(nsyl,gcm_decile) %>% 
  pivot_wider(names_from = gcm_decile, values_from = n)

ik3 = ik2 %>% 
  group_by(front,nsyl,gcm_decile) %>% 
  sample_n(10, replace = F)
  
vh2 %>% 
  count(vowel,gcm_decile) %>% 
  pivot_wider(names_from = gcm_decile, values_from = n)

vh3 = vh2 %>% 
  subset(vowel == 'e') %>% 
  group_by(gcm_decile) %>% 
  sample_n(30, replace = T) %>% # we overshoot the target, 20, because of replacement
  distinct()

vh3 = vh2 %>% 
  subset(vowel == 'é') %>% 
  group_by(gcm_decile) %>% 
  sample_n(10) %>% 
  distinct() %>% 
  bind_rows(vh3)

ep2 %>% 
  count(suffix1,suffix2,linking_vowel) %>% 
  pivot_wider(names_from = linking_vowel, values_from = n)

ep2 %>% 
  count(suffix1,suffix2,gcm_decile) %>% 
  pivot_wider(names_from = gcm_decile, values_from = n)

ep2 %>% 
  count(nsyl,gcm_decile) %>% 
  pivot_wider(names_from = gcm_decile, values_from = n)

ep3 = ep2 %>%
  filter(gcm_decile != 1) %>% 
  group_by(nsyl,gcm_decile) %>% 
  sample_n(18)

ep3 = ep2 %>%
  filter(gcm_decile == 1) %>% 
  group_by(gcm_decile) %>% 
  sample_n(36) %>% 
  bind_rows(ep3)


# -- write -- #

write_tsv(ik3, 'src/nonce_words/forms/ik_prompts.tsv')
write_tsv(vh3, 'src/nonce_words/forms/vh_prompts.tsv')
write_tsv(ep3, 'src/nonce_words/forms/ep_prompts.tsv')

# ik3 = read_tsv('src/nonce_words/forms/ik_prompts.tsv')
# vh3 = read_tsv('src/nonce_words/forms/vh_prompts.tsv')
# ep3 = read_tsv('src/nonce_words/forms/ep_prompts.tsv')

ik3 %>% 
  ungroup() %>% 
  sample_n(n()) %>% 
  arrange(gcm_decile,nsyl) %>%
  select(word,gcm_decile) %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'lakok/lakom')

vh3 %>% 
  ungroup() %>% 
  sample_n(n()) %>% 
  arrange(gcm_decile) %>%
  select(word,gcm_decile) %>% 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'dzsungelban/dzsungelben')

ep3 %>% 
  ungroup() %>% 
  sample_n(n()) %>% 
  arrange(gcm_decile,nsyl) %>%
  mutate(
    cvc = ifelse(
      str_detect(cc, 'lik$'), str_replace(cvc, 'ik$', ''), cvc)
  ) %>% 
  select(cc,cvc,gcm_decile) %>%
googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'cselekszik/cselekedik')
