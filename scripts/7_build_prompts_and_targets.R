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
  select(-v)

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
  select(word,type,fotelnak,fotelnek)

# ep: multiple suffixes

# define suffixes add labels
ep_suffixes = 
  tibble(
    ep_suffix = c('nak','ni','tak','na','tok'),
    type = c('cselekednek','cselekedni','cselekedtek','cselekedne','cselekedtek')
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
  select(cvc,cc,type,cselekednek,cselekszenek)

# -- shuffle -- #

ik_vars %<>% sample_n(n())
ep_vars %<>% sample_n(n())
vh_vars %<>% sample_n(n())

# -- write -- #

write_tsv(ik_vars, 'nonce_words/prompt/ik_prompts.tsv')
write_tsv(vh_vars, 'nonce_words/prompt/vh_prompts.tsv')
write_tsv(ep_vars, 'nonce_words/prompt/ep_prompts.tsv')
