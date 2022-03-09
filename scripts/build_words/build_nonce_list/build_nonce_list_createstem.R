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

# take out very rare constituents
n_ons = n2 %>% 
  count(onset, sort = T) %>% View
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

# we filter the frequency weighted lists
n_ons = n2 %>% 
  filter(onset %in% n_ons) %>% 
  pull(onset)
n_nuc = n2 %>% 
  filter(nucleus %in% n_nuc) %>% 
  pull(nucleus)
n_cod = n2 %>% 
  filter(coda %in% n_cod) %>% 
  pull(coda)

# we combine parts of words to create nonce words. filter for those that happen to exist. filter for very long ones, those tend to turn out silly. filter for some across-word sequences that are bad. é in heavy syllable, same character twice, not á and é word, word ending in ij/zj
n3 = 
  crossing(
    onset = n_ons,
    nucleus = n_nuc,
    coda = n_cod
  ) %>% 
  mutate(
    tr = glue('{onset}{nucleus}{coda}')
  ) %>% 
  filter(
    !(tr %in% h$tr),
    nchar(nucleus) >= 2,
    # nchar(tr) <= 7, # only cccvcvc or ccvccvc or cvccvcc or ccvcvcc  
    str_detect(tr, '(^ji|ch|é.{2,}$|(.)\\1+|[íóú].*é|[iz]j$|[eé]$|^[aáoóuú]|é(s|.t)$)', negate = T),
  ) %>% 
  mutate(
    word = transcribe(tr,'double')
  ) %>% 
  rowwise() %>% 
  filter(
    countUnique(word,1) # one letter character only once! otherwise words get silly
  ) %>% 
  ungroup()

# sample_n(n3, 10)
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

# combine bits to form nonce verbs. we don't need to worry about non-monomorphemic character combinations here, because a word with these derivational suffixes is by definition not monomorphemic. except no character twice, no Bszik and Tzik, no zzik, llik, sszik
# we add some more onsets because we only have simplex ones here.
v_1syl_2 = 
  crossing(
    onset = c(v_1syl$onset,c('sp','kl','tr','br','pr','spr','fl','fr','pl','dr','ßtr')),
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
    countUnique(word,2), # one character can happen twice, as I'm more mellow here
    str_detect(tr, '(^ji|ch|[sßfd]dik$|[bdgzΩß]ßik$|[ptksßz]zik$|llik$)', negate = T)
  ) %>% 
  ungroup()

# sample_n(v_1syl_2,10)
# # chef's kiss

## 2syl

# build 2-syl verbs
# filter for 2syl stems, drop igekötők, only keep derivational lik zik szik dik (new one!)
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
  rowwise() %>% 
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

# making and filtering frequency-weighted constituents
v2_v1c1c2 = v_2syl %>% 
  filter(c1c2 %in% v2_c1c2) %>% 
  pull(v1c1c2)
v2_v2c3c4 = v_2syl %>% 
  filter(c3c4 %in% v2_c3c4) %>% 
  pull(v2c3c4)

# combine bits to form new words
# add some more onsets
v_2syl_2 =
  crossing(
    onset = c(v_2syl$onset,c('sp','kl','tr','br','pr','spr','fl','fr','pl','dr','ßtr')),
    v1c1c2 = v2_v1c1c2,
    v2c3c4 = v2_v2c3c4
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
  mutate(
    vowel_skeleton = tr %>% 
      str_extract_all('[aáeéiíoóöőuúüű]') %>% 
      unlist() %>% 
      paste(collapse = ''),
    consonant_skeleton = tr %>% 
      str_extract_all('[^aáeéiíoóöőuúüű]') %>% 
      unlist() %>% 
      paste(collapse = '')
  ) %>% 
  filter(
    checkVH(tr),
    countUnique(word,2), # zoinks scoob
    str_count(word, 's') <= 1, # no szazs-type words
    str_count(word, 'z') <= 1, # no szazs-type words
    str_count(word, 'y') <= 1, # no gyaly-type words
    str_detect(vowel_skeleton, '(.)\\1+', negate = T), # no repeat cons or vow
    str_detect(consonant_skeleton, '(.)\\1+', negate = T),
    str_detect(tr, '(^ji|[íóú].*é|[uúüű][zdl]ik$)', negate = T)
  ) %>% 
  ungroup()

# sample_n(v_2syl_2,10)
# chef's kiss

# -- write -- #

write_tsv(n3, 'resource/nonce_words/nounstems.tsv')
write_tsv(v_1syl_2, 'resource/nonce_words/verbsshortstems.tsv')
write_tsv(v_2syl_2, 'resource/nonce_words/verbslongstems.tsv')
