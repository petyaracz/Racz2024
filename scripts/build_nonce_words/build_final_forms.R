## build prompts and targets from filtered nonce word lists

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)


# -- read -- #

ik = read_tsv('nonce_words/filt/nonce_ik_filt.txt')
vh = read_tsv('nonce_words/filt/nonce_vh_filt.txt')
ep = read_tsv('nonce_words/filt/nonce_ep_filt.txt')

# -- filter -- #

ik %<>% distinct()
vh %<>% distinct()
ep %<>% distinct()

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
  ungroup()

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
