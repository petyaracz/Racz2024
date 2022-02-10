## build three variable form lists from webcorpus2
# lakok/lakom
# cselekedik/cselekszik
# fotelben/fotelban

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(magrittr)

# -- read -- #

c = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')
h = read_tsv('src/hu_list.txt') %>% 
  pull(word)

hik = str_replace(h, 'ik$', '')

# tidy hand-cleaned ep verb list, made from output of predrawEP()
hanglemmata = read_tsv('src/epenthetic_stems/stemlist_from_webcorpus_cleared.tsv')

# -- def -- #

# the variable ik paradigm slot
xpostag_ik = '\\[\\/V\\]\\[Prs.NDef.1Sg\\]'
# five interesting, c-initial variable nominal suffixes (nouns that can be adjectives are marked as adjectives in the tags)
xpostags_n = '^(\\[\\/N\\]|\\[\\/Adj\\])(\\[Ine\\]|\\[Ill\\]|\\[Ade\\]|\\[Dat\\]|\\[Subl\\])$'
# five v-initial verbal conjugations to trigger verb ep
xpostags_ep = c("[/V][Prs.NDef.3Pl]","[/V][Inf]","[/V][Pst.NDef.3Pl]","[/V][Cond.NDef.3Sg]","[/V][Prs.NDef.2Pl]")

# -- functions -- #

buildPairs = function(dat){
  
  variants = unique(dat$variant)
  
  variant1 = dat %>% 
    filter(variant == variants[1]) %>% 
    select(form,freq,stem,xpostag,variation,lemma_freq_corrected,corpus_size) %>% 
    rename(
      'form_1' = form,
      'freq_1' = freq
    )
  
  variant2 = dat %>% 
    filter(variant == variants[2]) %>% 
    select(form,freq,stem,xpostag,variation,lemma_freq_corrected,corpus_size) %>% 
    rename(
      'form_2' = form,
      'freq_2' = freq
    )
  
  full_join(variant1,variant2, by = c("stem", "xpostag", "variation", "lemma_freq_corrected", "corpus_size")) %>%
    mutate(
      odds =  ( freq_1 + 1 ) / ( freq_2 + 1 ),
      log_odds = log(odds)
    )
  
}

drawIk = function(c,h,hik){

# 1sg indef ik forms can have the exponent -k or -m. There are two problems. The first problem: For some, though not all, -k variants, there is a separate lemma for the -k form and the -m form. This means you can't filter the list based on lemma. Second problem: Some -m forms are probably parsed as def even though they are indef. There's nothing I can do about this. I pull all 1sg indef ik verb forms from the webcorpus. I generate the -k and the -m endings. I search all 1sg indef verb forms for these, circumventing problem 1. Problem 2 is hopeless. We can only exclude forms that show a suspicious lack of -m variants.

# take c, grab 1sg indef for V lemmata ending in ik.
# some verbs might be -ik verbs on some cosmic plane but not be attested in the corpus with -ik lemmata. this search method misses out on those.
ik1 = c %>% 
  filter(
    str_detect(lemma, 'ik$'),
    str_detect(xpostag, xpostag_ik)
  )

# take the 1 sg indef for these forms w/o the variable ending (esze[km])
beginnings = ik1 %>% 
  select(form) %>% 
  mutate(
    beginning = form %>% 
           str_replace('.$',''),
    m_variant = glue('{beginning}m'),
    k_variant = glue('{beginning}k')
  ) %>% 
  select(-form) %>% 
  pivot_longer(-beginning, names_to = 'variant', values_to = 'form')

# find all -m and -k 1sg indef we got 
v1sgndef = c %>% 
  filter(
    str_detect(xpostag, '\\[\\/V\\]\\[Prs.NDef.1Sg\\]')
  )

ik1 = inner_join(beginnings,v1sgndef) %>% 
  distinct()

# mop up the lemmata a bit
ik2 = ik1 %>%
  filter(
    lemma %in% c(h,hik),
    nchar(lemma) > 2,
    !(lemma %in% c('kik','lik','mik','nek','sut','tok','víz','akik','előd','hísz','föld','mind','nyár','öreg','pénz','radi','szem','szik','egyik','előny','ladik','nekik','nékik','vissz'))
  )

# calculate corrected lemma freq for forms that have more than 1 lemma (e.g. bízik/bíz is the same lemma for our purposes)
ik_lemma = ik2 %>% 
  distinct(beginning,lemma,lemma_freq,corpus_size,xpostag) %>% 
  group_by(beginning,xpostag,corpus_size) %>% 
  summarise(lemma_freq_corrected = sum(lemma_freq)) %>% 
  ungroup()

# same for forms themselves
ik_form = ik2 %>% 
  group_by(beginning,form) %>% 
  summarise(freq = sum(freq))

ik3 = left_join(ik_lemma,ik_form, by = 'beginning')

# slap a category marker for the variants
ik3 %<>%
  mutate(
    stem = beginning,
    variation = 'lakom/lakok',
    variant = case_when(
      str_detect(form, 'm$') ~ 'lakom',
      str_detect(form, 'k$') ~ 'lakok'
    )
  )
return(ik3)
}

drawNoun = function(c){

# filter cfull for these suffixes
nouns = c %>% 
  filter(
    str_detect(xpostag, xpostags)
  )

 nouns %>% filter(lemma %in% c('dzsungel','norvég','hotel','fotel','határozott')) %>% View

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

return(nouns2)  
}

predrawEP = function(c,xpostags_ep){

# pre-compile list, no other way

 ik = c %>%
   filter(
     hunspell,
     xpostag == '[/V][Inf]',
     str_detect(lemma, '[rtpsdfghjklzxcvbnmy](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])ik$')
   ) %>%
   distinct(lemma)
 
 ik %>%
   arrange(lemma) %>%
   write_tsv('notes/ik.txt')
}

drawEP = function(hanglemmata,c){

	# we build all conceivable stem forms
hanglemmata %<>% 
  mutate(
    cvcik = str_replace(na, 'n[ae]$', 'ik'),
    cc = str_replace(ccik, 'ik$', ''),
    cvc = str_replace(cvcik, 'ik$', ''),
    stem = str_replace(cvc, '[aáeéiíoóöőuúüű](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])$', '')
  ) %>%
  select(-na)
  
hanglemmata2 = hanglemmata %>% 
  pivot_longer(-stem, names_to = 'stem_type', values_to = 'lemma')

# we rummage through the corpus for these stems
hang = inner_join(hanglemmata2,c, by = "lemma")

# since we have multiple "lemmata" per stem, we add up frequencies. we only keep variable forms we're interested in (the xpostags). we figure out whether a given form is cc (ugranak) or cvc (ugornak) using the magic of regular expressions.
hang2 = hang %>% 
  group_by(stem) %>% 
  mutate(
    lemma_freq_corrected = sum(lemma_freq)
  ) %>% 
  ungroup() %>% 
  filter(xpostag %in% postags) %>% 
  mutate(
    ending = str_replace(form, glue('^{stem}'), ''),
    type = case_when(
      str_detect(ending, '^[rtpsdfghjklzxcvbnm]') ~ 'CC',
      str_detect(ending, '^[aáeéiíoóöőuúüű]') ~ 'CVC'
    )
  ) %>% 
  arrange(stem,xpostag,type,-freq)

# some forms are weird. we remove them
hang2 %<>% 
  filter(
    !(form %in% c('aldni','gyanakdni','nyugdni','telepdni','veszekdni','vetekdni'))
  )

# the dataset has many typos and things like that. we keep the most frequent form for each variant to improve sanity. adding up the frequencies for variant sets would also be a way to go, if slightly iffy (it would argue for frequency tallies for some sort of abstract cvc / cc form (so that aludni and aluszni are the same thing)). not really what we do with the other two variables.
hang2 %<>% 
  select(stem,lemma,form,xpostag,freq,type,lemma_freq_corrected,corpus_size) %>%
  group_by(stem,xpostag,type) %>%
  slice(1)

# we only keep variable forms, as usual.  
hang2 %<>% 
  group_by(stem,xpostag) %>% 
  filter(n() == 2)

# getting round to pairing up
hang3 = hang2 %>% 
  mutate(
    variation = 'cselekszenek/cselekednek',
    variant = type
  )

return(hang3) 
}

# -- run -- #

## ik
# create ik list
ik = drawIk(c,h,hik)
# pair up -k and -m forms. since some -m forms are basically missing (they were probably missed by the morphological parser) we only include forms that vary within the 1sg indef.
ik2 = buildPairs(ik) %>%
  filter(!is.na(form_1) & !is.na(form_2))

## nouns
# create noun list
nouns = drawNoun(c)
# pair up forms. only keep variable forms
nouns3 = buildPairs(nouns2) %>%
  filter(!is.na(form_1) & !is.na(form_2))

## ep
# build list that I then fix by hand (see l23 in file)
# predrawEP(c,xpostags_ep)
# create verb list
ep = drawEP(c,hanglemmata)
ep2 = buildPairs(ep)

# -- write -- #

# save ik2 which is the tidy merged pair thing for ik verbs
write_tsv(ik2, '~/Github/Racz2024/src/ik_verbs/ikes_pairs_webcorpus2.tsv')
# save nouns2 which is the tidy merged pair thing for nouns
write_tsv(nouns3, 'src/front_harmony/fh_pairs_webcorpus2.tsv')
# save ep2 which is ~ for ep verbs
write_tsv(ep2, 'src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')