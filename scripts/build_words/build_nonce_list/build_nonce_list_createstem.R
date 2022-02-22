####################################
# create nonce forms for nouns, ik verbs
# for more info: build_nonce_list_functions
####################################

# -- header -- #

setwd('~/Github/Racz2024/')

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

# -- read -- #

v = read_tsv('resource/nonce_words/ik.tsv') # relevant real verbs
n = read_tsv('resource/nonce_words/dzsungel.tsv') # relevant real nouns
h = read_tsv('resource/hu_list.txt')

# -- wrangle -- #

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

# take out very rare contituents
n_ons = n2 %>% 
  count(onset, sort = T) %>% 
  filter(n >= 4) %>% 
  pull(onset)
n_nuc = n2 %>% 
  count(nucleus, sort = T) %>% 
  filter(n >= 2) %>% 
  pull(nucleus)
n_cod = n2 %>% 
  count(coda, sort = T) %>% 
  filter(n >= 3) %>% 
  pull(coda)

# we take out some combinations that downgrade realness of final words
n3 = n2 %>%
  filter(
#     nchar(mid) < 3, # no vcccv
#     str_detect(mid, '(.)\\1{1,}', negate = T), # midsection: no same cc
#     str_detect(mid, '(h|ly|^y$)', negate = T), # no.
#     str_detect(mid, '(¥l|vf|©r|çf|tß|[tsß][dzΩ]|[dzΩ][tsß]|[tdc][tdc]|cç|çc|n¥|[ßΩ][sz]|[sz][ßΩ]|g©)', negate = T), # no ssz, zzs, szs, zsz
#     # nchar(coda) == 2,
#     str_detect(coda, '(.)\\1{1,}', negate = T), # same for coda
#     str_detect(coda, '(h|ly)', negate = T),
#     str_detect(coda, '(n¥|[ßΩ][sz]|[sz][ßΩ]|g©)', negate = T),
#     str_detect(coda, '(rs|ly|d.|m.|k.|p.|rj|lm|lp|¥v|rv)$', negate = T), # a couple weird word endings
    onset %in% n_ons,
    nucleus %in% n_nuc,
    coda %in% n_cod,
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
    !(tr %in% h$tr),
    # nchar(tr) <= 7, # only cccvcvc or ccvccvc or cvccvcc or ccvcvcc  
    str_detect(tr, 'é.{2,}$', negate = T)
  ) %>% 
  mutate(
    word = transcribe(tr,'double')
  ) %>% 
  rowwise() %>% 
  filter(
    countUnique(word,1) # one letter character only once! otherwise words get silly
  ) %>% 
  ungroup()

# sample_n(n4, 10)
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
  )
  
# v_1syl$c1c2 %>% unique %>% sort
# v_1syl_2 = v_1syl %>% 
  # filter(nchar(c1c2) == 2) # vcik verbs look a lot like possessives w/ -om (lakom?!) so we drop those.

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
    countUnique(word,2) # one character can happen twice, as I'm more mellow here
  ) %>% 
  ungroup()

sample_n(v_1syl_2,10)
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
    c1c2 = glue('{c1}{c2}'),
    c3c4 = glue('{c3}{c4}'),
    v1c1c2 = glue('{v1}{c1}{c2}'),
    v2c3c4 = glue('{v2}{c3}{c4}')
  ) 

# rare constituents
v2_c1c2 = v_2syl %>%
  count(c1c2, sort = T) %>% 
  filter(n >= 3) %>% 
  pull(c1c2)
v2_c3c4 = v_2syl %>%
  count(c3c4, sort = T) %>% 
  filter(n >= 3) %>% 
  pull(c3c4)

# filtering w/ restrictions
v_2syl_2 = v_2syl %>% 
  filter(
    nchar(v2c3c4) >= 2,
    nchar(v1c1c2) >= 2,
    # str_detect(c3c4, '.t$', negate = T)
    c1c2 %in% v2_c1c2,
    c3c4 %in% v2_c3c4
  )

# combine bits to form new words
v_2syl_3 =
  crossing(
    onset = v_2syl_2$onset,
    v1c1c2 = v_2syl_2$v1c1c2,
    v2c3c4 = v_2syl_2$v2c3c4,
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
    countUnique(word,2), # zoinks scoob
    str_count(word, 's') <= 1, # no szazs-type words
    str_count(word, 'z') <= 1, # no szazs-type words
    str_count(word, 'y') <= 1 # no gyaly-type words
  ) %>% 
  ungroup()

# sample_n(v_2syl_3,10)
# chef's kiss

# -- write -- #

write_tsv(n4, 'resource/nonce_words/nounstems.tsv')
write_tsv(v_1syl_2, 'resource/nonce_words/verbsshortstems.tsv')
write_tsv(v_2syl_3, 'resource/nonce_words/verbslongstems.tsv')
