# in progress

## build prompts and targets from filtered nonce word lists

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# -- read -- #

ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
nouns = 

# -- functions -- #

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

# -- recycle bin -- #

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


nonce_ep %<>% 
  mutate(
    cvc = map_chr(words, ~ nth(.,1)),
    cc = map_chr(words, ~ nth(.,2))
  ) %>% 
  pivot_longer(-words, names_to = 'type', values_to = 'word') %>% 
  select(-words)


# -- filter shake -- #

# ik %<>% distinct() %>% sample_n(n())
# vh %<>% distinct() %>% sample_n(n())
# ep %>% 
#   distinct() %>% 
#   rownames_to_column() %>% 
#   mutate(
#     rowname = as.double(rowname),
#     word_id = case_when(
#       rowname %% 2 == 0 ~ rowname - 1, # science
#       rowname %% 2 == 1 ~ rowname
#     )
#   ) %>%
#   select(-rowname) %>%
#   filter(word_id != 637) %>% 
#   mutate(
#     type = case_when(
#       str_detect(word, '[aáeéiíoóöőuúüű](?=(sz|[^aáeéiíoóöőuúüű])ik)') ~ 'cvc',
#       str_detect(word, '[^aáeéiíoóöőuúüű](?=(sz|[^aáeéiíoóöőuúüű])ik)') ~ 'cc'
#     )
#   ) %>%
#   ungroup() %>% 
#   pivot_wider(-word_id, names_from = type, values_from = word, values_fn = list)
#   # sample_n(n())

# -- build -- #

# two points of order: (i) -Vlik doesn't exist. So epenthetic verb stems ending in l haven't got CVC -ik forms. (ii) the d/sz CVC/CC alternation only works with V-initial person/number suffixes. so 'aludnak/alszanak' is fine, 'aludna/*alszana' has no CC form. Taken together this means that not all nonce verbs should have all forms. -lik should only be prompted in CC (or we acknowledge that we're doing weird things that don't actually exist in Hungarian) and d/sz verbs should not be prompted with -ni and -na.

# ik: glue -m and -k at end of word, watch for linking vowel, which does rounding and front harmony
ik_vars = ik %>% 
  mutate(
    v = 
      case_when(
        str_detect(word, '[öőüű](?=[^aáeéiíoóöőuúüű]+ik$)') ~ 'ö',
        str_detect(word, '[eéií](?=[^aáeéiíoóöőuúüű]+ik$)') ~ 'e',
        str_detect(word, '[aáoóuú](?=[^aáeéiíoóöőuúüű]+ik$)')~ 'o'
      ),
    lakok = str_replace(word, 'ik$', glue('{v}k')),
    lakom = str_replace(word, 'ik$', glue('{v}m'))
  ) %>% 
  select(-v) %>% 
  ungroup()

# vh: multiple suffixes. 

# define suffixes and add labels
vh_suffixes = 
  tibble(
    vh_suffix = c('bvn','bv','vn','nvk','rv'),
    type = c('fotelban','fotelba','fotelon','fotelnak','fotelra')
  )

# cross suffixes with forms. create appropriate suffix forms, glue them together
vh_vars = vh %>% 
  crossing(
    vh_suffixes
  ) %>% 
  mutate(
    front_suffix = str_replace(vh_suffix, 'v', 'e'),
    back_suffix = ifelse(vh_suffix == 'vn', str_replace(vh_suffix, 'v', 'o'), str_replace(vh_suffix, 'v', 'a')),
    fotelnak = glue('{word}{back_suffix}'),
    fotelnek = glue('{word}{front_suffix}') 
  ) %>% 
  select(word,type,fotelnak,fotelnek) %>% 
  ungroup() %>% 
  right_join(vh, ., by = 'word')

# ep: multiple suffixes

# define suffixes add labels
ep_suffixes = 
  tibble(
    ep_suffix = c('nak','ni','tak','na','tok'),
    type = c('ők cselekednek','cselekedni','ők cselekedtek','ő cselekedne','ti cselekedtek')
  )

# phew. cross forms with suffixes. figure out last vowel of stem which we need for suffix harmony. figure out suffix harmony. figure out linking vowel. figure out stem. create variants. fix past tense -tak/tek.
ep_vars = ep %>% 
  crossing(
    ep_suffixes
  ) %>% 
  mutate(
    v_last = str_extract(cc, '[aáeéíioóöőuúüű](?=[^aáeéíioóöőuúüű]+ik$)'),
    real_suffix = case_when(
      str_detect(v_last, '[eéiíöőüű]') ~ str_replace(ep_suffix, '[ao]', 'e'),
      str_detect(v_last, '[iíöőüű]') ~ str_replace(ep_suffix, 'o', 'ö'),
      T ~ ep_suffix
      ),
    linking_vowel = case_when(
      str_detect(real_suffix, 'a') ~ 'a',
      str_detect(real_suffix, 'o') ~ 'o',
      str_detect(real_suffix, 'e') ~ 'e',
      str_detect(real_suffix, 'ö') ~ 'ö',
      str_detect(real_suffix, 'i') & str_detect(v_last, '[eéiíöőüű]') ~ 'e',
      str_detect(real_suffix, 'i') & str_detect(v_last, '[aáoóuú]') ~ 'a'
    ),
    cc_stem = str_replace(cc, 'ik', ''),
    cvc_stem = str_replace(cvc, 'ik', ''),
    cselekednek = glue('{cvc_stem}{real_suffix}'),
    cselekszenek = glue('{cc_stem}{linking_vowel}{real_suffix}') %>% 
      str_replace('atak$','ottak') %>% 
      str_replace('etek$','ettek')
    ) %>% 
  select(cvc,cc,type,cselekednek,cselekszenek) %>% 
  ungroup()

# -- shuffle -- #

ik_vars %<>% sample_n(n())
ep_vars %<>% sample_n(n()) 
vh_vars %<>% sample_n(n())

# -- write -- #

write_tsv(ik_vars, 'nonce_words/prompt/ik_prompts.tsv')
write_tsv(vh_vars, 'nonce_words/prompt/vh_prompts.tsv')
write_tsv(ep_vars, 'nonce_words/prompt/ep_prompts.tsv')

# -- google sheet -- #

gs_ik = ik_vars %>% 
  sample_n(200)

gs_vh = vh_vars %>% 
  distinct(word) %>% 
  sample_n(200) %>% 
  left_join(vh_vars)

gs_ep = ep_vars %>% 
  distinct(cc) %>% 
  sample_n(200) %>% 
  left_join(ep_vars)

stems = bind_cols(
  ik = gs_ik$word,
  vh = unique(gs_vh$word),
  ep_cc = unique(gs_ep$cc),
  ep_cvc = unique(gs_ep$cvc)[1:200]
)
  
googlesheets4::write_sheet(gs_ik, 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'ik')
googlesheets4::write_sheet(gs_ep, 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'ep')
googlesheets4::write_sheet(gs_vh, 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'vh')
googlesheets4::write_sheet(stems, 'https://docs.google.com/spreadsheets/d/1Iximhc57X1yYPwtm10ubvYfNaT7YL7jje5RkOilUAPw/edit?usp=sharing', sheet = 'stems')
