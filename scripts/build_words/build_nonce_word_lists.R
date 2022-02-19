####################################
# Build nonce word lists from scratch
# @raczpetya
####################################

# code logic:
# 1. draw 2-syl nouns and ik verbs from frequency list
# 2. break noun and verb forms into pieces and recombine those into new nonce words. do this separately for nouns, 1syl verbs, 2 syl verbs.
# 3. draw some forms at random from these lists, filter them across similarity to existing words, similarity to each other.
# 4. build epenthetic verb stems. (nouns and ik verbs are already there anyway!)

## in: c, the webcorpus frequency list I made. xpostags for each var.
## out: nonce forms with endings for the psychopy script
## general notes
# the code is broken up into four parts. part one involves indexing a very large file and part three takes ages. which part is active is controlled by four vars (build1-4) that are on by default meaning that no parts are active and so running the code does nothing.
# the starting points are bisyllabic mixed V nouns (makes sense), and separately mono and bisyllabic verbs. words are taken apart into chunks that take phonotactics into account (word onset, vowel with all the C after it) and then combined freely. this results in stem V combinations that don't really exist (e+ö etc) so those are filtered out.

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# -- code control -- #

build1 = T # real word lists already built
build2 = T # nonce word lists already built
build3 = T # nonce word lists already filtered
build4 = T # final combinations built, epenthetic stems built
 
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
    ( str_detect(v1, '[aáoóuűúií]') & str_detect(v2, '[aáoóuűúií]') ) |
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

# -- read -- #

# spelling dictionary
h = read_tsv('resource/hu_list.txt')

if ( !build1 ){ 
  c = read_tsv('resource/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz') # frequency list. big.
} 
if ( !build2 ){
  v = read_tsv('resource/nonce_words/ik.tsv') # relevant real verbs
  n = read_tsv('resource/nonce_words/dzsungel.tsv') # relevant real nouns
}
if ( !build3 ){
  n = read_tsv('resource/nonce_words/nouns1.tsv') # nonce words built from real words
  v1 = read_tsv('resource/nonce_words/verbsshort1.tsv')
  v2 = read_tsv('resource/nonce_words/verbslong1.tsv')
}
if ( !build4 ){
  n = read_tsv('resource/nonce_words/nouns_filt.tsv') # filtered nonce word lists
  v1 = read_tsv('resource/nonce_words/verbs_short_filt.tsv')
  v2 = read_tsv('resource/nonce_words/verbs_long_filt.tsv')
  ep = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv') # real epenthetic verbs for reference
}

####################################
# draw relevant forms for nouns, ik verbs
####################################

if ( !build1 ){
  
  # -- wrangle -- #
  
  # all bisyllabic nouns
  n = c %>% 
    buildList2() %>% 
    filter(
      xpostag == '[/N][Nom]',
      str_count(lemma, '[aáeéiíoóöőuúüű]') == 2
    )
  
  # all ik verbs
  v = c %>% 
    buildList2() %>% 
    filter(
      xpostag == '[/V][Prs.NDef.3Sg]',
      str_detect(lemma, 'ik$')
    )
  
  # extract variable stems (back V + e/é)
  n2 = n %>%
    rowwise() %>% 
    mutate(
      vowels = str_extract_all(lemma, '[aáeéiíoóöőuúüű]'),
      v1 = vowels[[1]],
      v2 = vowels[[2]]
    ) %>% 
    filter(
      str_detect(v1, '[aáoóuúií]'),
      str_detect(v2, '[eé]')
    )
  
  # -- write -- #
  
  write_tsv(v, 'resource/nonce_words/ik.tsv')
  write_tsv(n2, 'resource/nonce_words/dzsungel.tsv')
  
}

####################################
# draw relevant forms for nouns, ik verbs
####################################

if ( !build2 ){
  
  # -- build nouns -- #
  
  # take words apart. we keep vowel 1 + following consonants together to make sure we don't roll impossible combinations (e.g. óCC+) later. 
  n2 = n %>% 
    select(lemma,v1,v2) %>% 
    mutate(
      tr = transcribe(lemma,'single'),
      onset = ifelse(
        !str_detect(tr, '^[^iáoóaíúu]+(?=[iáoóaíúu])'), 
        '',
        str_extract(tr, '^[^iáoóaíúu]+(?=[iáoóaíúu])')
      ),
      coda = str_extract(tr, glue('(?<={v2}).*$')),
      coda = glue('{v2}{coda}'),
      nucleus = str_extract(tr, glue('(?<=^{onset}).*(?={coda}$)')),
      mid = str_extract(nucleus, glue('(?<=^{v1}).*$'))
    )
  
  # we take out some combinations that downgrade realness of final words
  n3 = n2 %>% 
    filter(
      nchar(mid) < 3, # no vcccv
      str_detect(mid, '(.)\\1{1,}', negate = T), # midsection: no same cc
      str_detect(mid, '(h|ly|^y$)', negate = T), # no.
      str_detect(mid, '([tsß][dzΩ]|[dzΩ][tsß]|[tdc][tdc]|cç|çc|n¥|[ßΩ][sz]|[sz][ßΩ]|g©)', negate = T), # no ssz, zzs, szs, zsz
      # nchar(coda) == 2,
      str_detect(coda, '(.)\\1{1,}', negate = T), # same for coda
      str_detect(coda, '(h|ly)', negate = T),
      str_detect(coda, '(n¥|[ßΩ][sz]|[sz][ßΩ]|g©)', negate = T),
      str_detect(coda, '(ly|d.|m.|k.|p.|rj|lm|lp|¥v|rv)$', negate = T), # a couple weird word endings
      nucleus != v1, # no vv word
      coda != v2 # no v$ word
    )
  
  # n4$mid %>% unique %>% sort
  
  # we combine parts of words to create nonce words. filter for those that happen to exist. filter for very long ones, those tend to turn out silly.
  n4 = 
    crossing(
      onset = n3$onset,
      nucleus = n3$nucleus,
      coda = n3$coda
    ) %>% 
    mutate(
      tr = glue('{onset}{nucleus}{coda}')
    ) %>% 
    filter(
      !(tr %in% h),
      nchar(tr) <= 7, # only cccvcvc or ccvccvc or cvccvcc or ccvcvcc  
      str_detect(tr, 'é.{2,}$', negate = T)
    ) %>% 
    mutate(
      word = transcribe(tr,'double')
    ) %>% 
    rowwise() %>% 
    filter(
      countUnique(word,1) # zoinks scoob
    ) %>% 
    ungroup()
  
  # n5 %>% 
  #   sample_n(10)
  # # chef's kiss
  
  # -- build verbs -- #
  
  ## 1syl
  
  # pick 1syl ik verbs
  v_1syl = v %>% 
    select(lemma) %>% 
    filter(
      !(lemma %in% c('hangzik', 'hallszik')), # only 1syl stem verbs with ccc in mid
      str_count(lemma, '[aáeéiíoóöőuúüű]') == 2
    )
  
  # take them apart
  v_1syl %<>% 
    mutate(
      tr = transcribe(lemma,'single'),
      onset = ifelse(
        !str_detect(tr, '^[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'), 
        '',
        str_extract(tr, '^[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])')
      ),
      v = str_extract(tr, glue('(?<={onset})[aáeéiíoóöőuúüű]')),
      c1 = str_extract(tr, glue('(?<=^{onset}{v}).')),
      c2 = ifelse(
        !str_detect(tr, glue('(?<=^{onset}{v}{c1})[^aáeéiíoóöőuúüű]')),
        '',
        str_extract(tr, glue('(?<=^{onset}{v}{c1})[^aáeéiíoóöőuúüű]'))
      ),
      c1c2 = glue('{c1}{c2}')
    ) %>% 
    filter(nchar(c1c2) == 2) # I'm serious
  
  # combine bits to form nonce verbs
  v_1syl_2 = 
    crossing(
      onset = v_1syl$onset,
      v = v_1syl$v,
      c1c2 = v_1syl$c1c2
    ) %>% 
    mutate(
      tr = glue('{onset}{v}{c1c2}ik')
    ) %>% 
    filter(
      !(tr %in% h)
    ) %>% 
    mutate(
      word = transcribe(tr,'double')
    ) %>% 
    rowwise() %>% 
    filter(
      countUnique(word,2) # zoinks scoob
    ) %>% 
    ungroup()
  
  # v_1syl_2 %>% 
  #   sample_n(10)
  # # chef's kiss

  ## 2syl
  
  # build 2-syl verbs
  v_2syl = v %>% 
    select(lemma) %>% 
    filter(
      !(lemma %in% c('ódzkodik', 'kontrázik', 'pendlizik', 'jachtozik')), # only 2syl stem verbs with ccc in mid
      str_count(lemma, '[aáeéiíoóöőuúüű]') == 3,
      str_detect(lemma, '^(meg|le|fel|be|ki|szét|rá)', negate = T),
      str_detect(lemma, '(.)\\1{1,}', negate = T)
    )
  
  # take them apart
  v_2syl %<>% 
    mutate(
      tr = transcribe(lemma,'single'),
      onset = ifelse(
        !str_detect(tr, '^[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'), 
        '',
        str_extract(tr, '^[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])')
      ),
      v1 = str_extract(tr, glue('(?<=^{onset})[aáeéiíoóöőuúüű]')),
      c1 = ifelse(
        !str_detect(tr, glue('(?<=^{onset}{v1})[^aáeéiíoóöőuúüű]')),
        '',
        str_extract(tr, glue('(?<=^{onset}{v1})[^aáeéiíoóöőuúüű]'))
      ),
      c2 = ifelse(
        !str_detect(tr, glue('(?<=^{onset}{v1}{c1})[^aáeéiíoóöőuúüű]')),
        '',
        str_extract(tr, glue('(?<=^{onset}{v1}{c1})[^aáeéiíoóöőuúüű]'))
      ),
      v2 = str_extract(tr, glue('(?<=^{onset}{v1}{c1}{c2})[aáeéiíoóöőuúüű]')),
      c3 = ifelse(
        !str_detect(tr, glue('(?<=^{onset}{v1}{c1}{c2}{v2})[^aáeéiíoóöőuúüű]')),
        '',
        str_extract(tr, glue('(?<=^{onset}{v1}{c1}{c2}{v2})[^aáeéiíoóöőuúüű]'))
      ),
      c4 = ifelse(
        !str_detect(tr, glue('(?<=^{onset}{v1}{c1}{c2}{v2}{c3})[^aáeéiíoóöőuúüű]')),
        '',
        str_extract(tr, glue('(?<=^{onset}{v1}{c1}{c2}{v2}{c3})[^aáeéiíoóöőuúüű]'))
      ),
      c3c4 = glue('{c3}{c4}'),
      v1c1c2 = glue('{v1}{c1}{c2}'),
      v2c3c4 = glue('{v2}{c3}{c4}')
    ) 
  
  # some restrictions
  v_2syl %<>% 
    filter(
      nchar(v2c3c4) >= 2,
      nchar(v1c1c2) >= 2,
      str_detect(c3c4, '.t$', negate = T)
    )
  
  # combine bits to form new words
  v_2syl_2 = 
    crossing(
      onset = v_2syl$onset,
      v1c1c2 = v_2syl$v1c1c2,
      v2c3c4 = v_2syl$v2c3c4,
    ) %>% 
    mutate(
      tr = glue('{onset}{v1c1c2}{v2c3c4}ik')
    ) %>% 
    filter(
      !(tr %in% h),
      nchar(tr) <= 9 # ccvccvc or ccvcvcc or cvccvcc or cccvcvc + ik
    ) %>% 
    mutate(
      word = transcribe(tr,'double')
    ) %>% 
    rowwise() %>% 
    filter(
      checkVH(tr),
      countUnique(word,2) # zoinks scoob
    ) %>% 
    ungroup()
  
  # v_2syl_2 %>% 
  #   sample_n(10)
  # chef's kiss
  
  # -- write -- #
  
  write_tsv(n5, 'resource/nonce_words/nouns1.tsv')
  write_tsv(v_1syl_2, 'resource/nonce_words/verbsshort1.tsv')
  write_tsv(v_2syl_2, 'resource/nonce_words/verbslong1.tsv')
}

####################################
# filter words based on similarity to real words, each other
####################################

if ( !build3 ){
  
  # how many words do we start with
  n_sample = 576 # <3
  
  # relevant comparison set for 2syl nouns and ik verbs
  h_n_v2 = h %>% 
    filter(nchar(tr) > 2, nchar(tr) <= 9) %>% 
    pull(tr)
  
  # relevant set for monosyllabic verbs
  h_v1 = h %>% 
    filter(nchar(tr) > 2, nchar(tr) <= 6) %>% 
    pull(tr)
  
  # relevant sets for margin search (short existing words)
  h_margins = h %>% 
    filter(nchar(tr) >= 2, nchar(tr) <= 4) %>% 
    pull(tr)
  
  # randomise word order and pick the n first ones = random draw
  n2 = n %>%
    sample_n(n()) %>%
    mutate(target = tr) %>%
    slice_head(n = n_sample)
  
  v12 = v1 %>% 
    sample_n(n()) %>% 
    mutate(target = str_replace(tr, 'ik$', '')) %>% # add ik
    slice_head(n = n_sample)
  
  v22 = v2 %>% 
    sample_n(n()) %>% 
    mutate(target = str_replace(tr, 'ik$', '')) %>% 
    slice_head(n = n_sample)
  
  # -- filter -- #
  
  # we use furrr to parallelise code. 
  plan(multisession, workers = 8)
  
  # n
  glue('doing nouns...')
  
  # map through nouns. check for distance and overlap. create boolean cols. time it.
  tictoc::tic('nouns')
  n3 = n2 %>%
    rowwise() %>%
    mutate(
      keep_enough_distance = future_map_lgl(target, ~ matchReal(., h_n_v2, 2)),
      keep_no_overlap = future_map_lgl(target, ~ matchMargins(., h_margins))
    )
  tictoc::toc()
  
  ## v1
  glue('doing short verbs...')
  
  # map and time monosyl verbs.
  tictoc::tic('short verbs')
  v13 = v12 %>%
    rowwise() %>% 
    mutate(
      keep_enough_distance = future_map_lgl(tr, ~ matchReal(., h_v1, 1)),
      keep_no_overlap = future_map_lgl(target, ~ matchMargins(., h_margins))
    )
  tictoc::toc()
  
  ## v2
  glue('doing long verbs...')
  
  # map and time bisyllabic verbs
  tictoc::tic('long verbs')
  v23 = v22 %>%
    rowwise() %>% 
    mutate(
      keep_enough_distance = future_map_lgl(tr, ~ matchReal(., h_n_v2, 2)),
      keep_no_overlap = future_map_lgl(target, ~ matchMargins(., h_margins))
    )
  tictoc::toc()
  
  # filtering based on similarity to other nonce words in group
  nw = n3 %>% 
    filter(keep_enough_distance,keep_no_overlap) %>% 
    pull(tr) %>% 
    matchNonce()
  
  n4 = n3 %>% 
    filter(tr %in% nw)
  
  v1w = v13 %>% 
    filter(keep_enough_distance,keep_no_overlap) %>% 
    pull(tr) %>% 
    matchNonce()
  
  # apparently this just filters almost all verbs out.
  # v14 = v13 %>% 
  #   filter(tr %in% v1w)
  v14 = v13 %>% 
    filter(keep_enough_distance,keep_no_overlap)
  
  v2w = v23 %>% 
    filter(keep_enough_distance,keep_no_overlap) %>% 
    pull(tr) %>% 
    matchNonce()
  
  v24 = v23 %>% 
    filter(tr %in% v2w)
  
  # -- write -- #
  
  write_tsv(n4, 'resource/nonce_words/nouns_filt.tsv')
  write_tsv(v14, 'resource/nonce_words/verbs_short_filt.tsv')
  write_tsv(v24, 'resource/nonce_words/verbs_long_filt.tsv')
}

####################################
# build final forms
####################################

if ( !build4 ){

  v1 = filter(v1, str_detect(word, '(zz|ll|dd)', negate = T))
  v2 = filter(v2, str_detect(word, '(zz|ll|dd)', negate = T))
  
  # first half of each set goes into eszek/eszem. second half to cselekedik/cselekszik
  v1a = v1[1:(nrow(v1)/2),]
  v2a = v2[1:(nrow(v2)/2),]
  v1b = v1[((nrow(v1)/2)+1):nrow(v1),]
  v2b = v2[(round((nrow(v2)/2))+1):nrow(v2),]
  
  ## nouns
  
  # we want these suffixes
  suffix_back = c('ban', 'ba', 'nak', 'ra', 'nál')
  
  # we combine stems with suffixes, generate the two alternatives, and then randomly pick a suffix for each word.
  n2 = n %>% 
    select(word) %>% 
    rename('prompt' = word) %>% 
    crossing(suffix_back) %>% 
    mutate(
      suffix_front = str_replace_all(suffix_back, c('ban$' = 'ben', 'ba' = 'be', 'nak' = 'nek', 'ra$' = 're', 'nál$' = 'nél')),
      target_front = glue('{prompt}{suffix_front}'),
      target_back = glue('{prompt}{suffix_back}')
      ) %>% 
    group_by(prompt) %>% 
    sample_n(1) %>% 
    ungroup()
  
  ## verbs
  
  # ik
  
  # take eszek/eszem, figure out linking vowel, generate targets
  # 1syl
  v1a2 = v1a %>% 
    select(v,word) %>% 
    rename('prompt' = word) %>% 
    mutate(
      suffix_v = case_when(
        str_detect(v, '[aáiíoóuú]') ~ 'o',
        str_detect(v, '[üűöő]') ~ 'ö',
        str_detect(v, '[eé]') ~ 'e'
           ),
      target_k = str_replace(prompt, 'ik', glue('{suffix_v}k')),
      target_m = str_replace(prompt, 'ik', glue('{suffix_v}m'))
    ) %>% 
    select(prompt,target_k,target_m)
  
  # 2syl
  v2a2 = v2a %>% 
    rename('prompt' = word) %>% 
    mutate(
      v = str_extract(v2c3c4, '[aáeéiíoóöőuúüű]'),
      suffix_v = case_when(
        str_detect(v, '[aáiíoóuú]') ~ 'o',
        str_detect(v, '[üűöő]') ~ 'ö',
        str_detect(v, '[eé]') ~ 'e'
      ),
      target_k = str_replace(prompt, 'ik', glue('{suffix_v}k')),
      target_m = str_replace(prompt, 'ik', glue('{suffix_v}m'))
    ) %>% 
    select(prompt,target_k,target_m)
  
  v_ik = bind_rows(v1a2,v2a2)
  
  # ep
  
  ep_pieces = ep %>% 
    mutate(
      form_1_tr = transcribe(form_1, 'single'),
      form_2_tr = transcribe(form_2, 'single'),
      stem_tr = transcribe(stem, 'single'),
      c1 = str_extract(stem_tr, '.$'),
      c2 = str_extract(form_1_tr, glue('(?<={stem_tr}).')),
      epv = str_extract(form_2_tr, glue('(?<={stem_tr}).')),
      c3 = str_extract(form_2_tr, glue('(?<={stem_tr}{epv}).'))
    ) %>% 
    distinct(stem,epv,c1,c2,c3) %>% 
    select(-stem)
  
  ep_pieces
  # "u" "o" "ö" "e" "ü" "a"
  
  # ep %>% 
  #   sample_n(n()) %>% 
  #   group_by(xpostag) %>% 
  #   slice_head(n = 1) %>% 
  #   select(stem,form_1,form_2,xpostag)
    
  # we want these suffixes
  suffix_ep = c('na', 'ni', 'tok', 'nak', 'tak')
  
  v1b2 = v1b %>% 
    mutate(stem = glue('{onset}{v}')) %>% 
    select(stem)
  
  v2b2 = v2b %>% 
    mutate(
      v = str_extract(v2c3c4, '^.'),
      stem = glue('{onset}{v1c1c2}{v}')
      ) %>% 
    select(stem,v) 
  
  v_ep = bind_rows(v1b2,v2b2) %>% 
    crossing(ep_pieces) %>% 
    filter(
      str_detect(stem, '[uoúó]') & str_detect(epv, '[uo]') |
        str_detect(stem, '[üöűő]') & str_detect(epv, '[üö]') |
        str_detect(stem, '[eé]') & str_detect(epv, '[e]') |
        str_detect(stem, '[aá]') & str_detect(epv, '[a]')
    ) %>% 
    mutate(
      prompt_cc_tr = glue('{stem}{c1}{c2}ik'),
      prompt_cvc_tr = glue('{stem}{c1}{epv}{c3}ik'),
      prompt_cc = transcribe(prompt_cc_tr, 'double'),
      prompt_cvc = transcribe(prompt_cvc_tr, 'double'),
      prompt_cvc_bare = str_replace(prompt_cvc, 'ik', ''),
      prompt_cc_bare = str_replace(prompt_cc, 'ik', '')
    ) %>% 
    select(prompt_cc,prompt_cvc,prompt_cc_bare,prompt_cvc_bare,epv) %>% 
    crossing(suffix_ep) %>%  
    mutate(
      suffix_ep_vh = case_when(
        str_detect(epv, '[aáiíoóuú]') & str_detect(suffix_ep, '(na|nak|tak|na)') ~ 'a',
        str_detect(epv, '[eéöőüű]') & str_detect(suffix_ep, '(na|nak|tak|na)') ~ 'e',
        str_detect(epv, '[aáiíoóuú]') & suffix_ep == 'tok' ~ 'o',
        str_detect(epv, '[öőüű]') & suffix_ep == 'tok' ~ 'ö',
        str_detect(epv, '[eéöőüű]') & suffix_ep == 'tok' ~ 'e',
        suffix_ep == 'ni' ~ 'i'
      ),
      suffix_ep_f = str_replace(suffix_ep, '[aio]', suffix_ep_vh),
      target_cc = glue('{prompt_cc_bare}{suffix_ep_f}'),
      target_cvc = glue('{prompt_cvc_bare}{suffix_ep_f}')
    ) %>% 
    select(prompt_cc,prompt_cvc,suffix_ep,target_cc,target_cvc) %>% 
    group_by(prompt_cc,prompt_cvc) %>% 
    sample_n(1) %>% 
    ungroup()
  
  # count(v_ep,suffix_ep)
  
  # -- write -- #
  
  # dzsungelban
  write_tsv(n2, 'resource/nonce_words/nouns_final.tsv')
  # eszek
  write_tsv(v_ik, 'resource/nonce_words/ik_final.tsv')
  # cselekszik
  write_tsv(v_ep, 'resource/nonce_words/ep_final.tsv')
}

if(all(build1,build2,build3,build4)){ browseURL('https://alchetron.com/cdn/tom-hutchinson-english-teacher-c7c80cfa-5200-42e6-b2d6-833b212d110-resize-750.jpg') }
