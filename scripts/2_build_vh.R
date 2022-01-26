## build front/back harmony list from webcorpus2 ##

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# -- list builder -- #

# I selected five consonant-initial nominal suffixes and searched for these in the webcorpus. Results were narrowed down to consonant-final bisyllabic lemmata with a back vowel and e/é with a lemma frequency of at least ten. Lemma-suffixs pairs that showed variation in the webcorpus (had at least two variable forms: dzsungelban/dzsungelben) were tallied up and log odds calculated.

# five interesting, c-initial variable nominal suffixes (nouns that can be adjectives are marked as adjectives in the tags)
xpostags = '^(\\[\\/N\\]|\\[\\/Adj\\])(\\[Ine\\]|\\[Ill\\]|\\[Ade\\]|\\[Dat\\]|\\[Subl\\])$'

# filter cfull for these suffixes
nouns = c %>% 
  filter(
    str_detect(xpostag, xpostags)
  )

# nouns %>% filter(lemma %in% c('dzsungel','norvég','hotel','fotel','határozott')) %>% View

# get rid of very odd/obnoxious lemmata. Then pluck out vowels from lemma. Then filter for lemma ending in C (not vowel) and a bisyllabic lemma skeleton of back vowel + e/é.
nouns2 = nouns %>% 
  filter(
    nchar(lemma) > 1,
    lemma_freq > 10,
  ) %>% 
  rowwise() %>% 
  mutate(
    form_vowels = form %>% 
      str_extract_all('[aáeéiíoóöőuúüű]', simplify = T) %>% 
      paste(collapse = ''),
    lemma_vowels = lemma %>% 
      str_extract_all('[aáeéiíoóöőuúüű]', simplify = T) %>% 
      paste(collapse = '')
  ) %>% 
  filter(
    str_detect(lemma, '[^aáeéiíoóöőuúüű]$'),
    str_detect(lemma_vowels, '^[aáoóuú][eé]$')
  )

# now count the number of variants per form, mark variant type (a or e), drop non-variable lemma-suffix pairings
nouns2 %<>% 
  group_by(lemma,xpostag) %>% 
  mutate(
    final_vowel = str_extract(form_vowels, '.$'),
    variant = case_when(
      final_vowel %in% c('a','á') ~ 'a',
      final_vowel %in% c('e','é') ~ 'e'
      ),
    n_variants = n()
    ) %>% 
  filter(n_variants > 1) %>% 
  ungroup()

# add cols expected by function
nouns2 %<>% 
  mutate(
    variation = 'hotelban/hotelben'
  ) %>% 
  rename(
    'stem' = lemma,
    'lemma_freq_corrected' = lemma_freq
  )

# -- build pairs -- #

nouns3 = buildPairs(nouns2)

# keep only variable forms
nouns3 %<>%
  filter(!is.na(form_1) & !is.na(form_2))

# -- write -- #

write_tsv(nouns3, 'src/front_harmony/fh_pairs_webcorpus2.tsv')

# -- quality control -- #

# I plucked out the same tallies from the old webcorpus frequency list (see regex above in readin). I tallied pairs, calced log odds, merged with new log odds from webcorpus2.
old %<>% 
  rowwise() %>% 
  mutate(
    word_vowels = word %>% 
      str_extract_all('[aáeéiíoóöőuúüű]', simplify = T) %>% 
      paste(collapse = ''),
    final_word_vowel = str_extract(word_vowels, '.$'),
    variant = case_when(
      final_word_vowel %in% c('a','á') ~ 'a',
      final_word_vowel %in% c('e','é') ~ 'e'
    )
  ) %>% 
  ungroup()

# build variant sets
olda = old %>% 
  filter(variant == 'a') %>% 
  select(word,freq,lemma,lemmafreq,analysis) %>% 
  rename('a_form' = word, 'a_freq' = freq)

olde = old %>% 
  filter(variant == 'e') %>% 
  select(word,freq,lemma,lemmafreq,analysis) %>% 
  rename('e_form' = word, 'e_freq' = freq) 

# merge variant sets, replace na with 0, tidy up tags for merge, remove lemmata not in spellcheck.
old2 = full_join(olda,olde, by = c("lemma", "lemmafreq", "analysis")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(
    a_e_odds_w1 =  ( a_freq + 1 ) / ( e_freq + 1 ),
    a_e_log_odds_w1 = log(a_e_odds_w1),
    tag = str_extract(analysis, '...(?=>>$)') 
  ) %>% 
  filter(
    lemma %in% hu$word
  )

unique(old2$tag)

# tidy up tag for merge in new pairs, combine w/ old pairs
old2 = nouns3 %>% 
  mutate(
    tag = str_extract(xpostag, '(?<=\\]\\[).*(?=\\]$)') %>% 
      str_to_upper() %>% 
      str_replace('SUBL','SBL')
           ) %>% 
  select(lemma,tag,a_e_log_odds) %>% 
  right_join(old2,., by = c("lemma", "tag"))

# yes this is good
with(old2, cor.test(a_e_log_odds,a_e_log_odds_w1))

# excellent
old2 %>% 
  ggplot(aes(a_e_log_odds,a_e_log_odds_w1,label = lemma)) +
  geom_text() +
  theme_few()


