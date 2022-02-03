## build -ik list from webcorpus2 ##

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(magrittr)

# -- list builder -- #

# 1sg indef ik forms can have the exponent -k or -m. There are two problems. The first problem: For some, though not all, -k variants, there is a separate lemma for the -k form and the -m form. This means you can't filter the list based on lemma. Second problem: Some -m forms are probably parsed as def even though they are indef. There's nothing I can do about this. I pull all 1sg indef ik verb forms from the webcorpus. I generate the -k and the -m endings. I search all 1sg indef verb forms for these, circumventing problem 1. Problem 2 is hopeless. We can only exclude forms that show a suspicious lack of -m variants.

# take c, grab 1sg indef for V lemmata ending in ik.
# some verbs might be -ik verbs on some cosmic plane but not be attested in the corpus with -ik lemmata. this search method misses out on those.
ik1 = c %>% 
  filter(
    str_detect(lemma, 'ik$'),
    str_detect(xpostag, '\\[\\/V\\]\\[Prs.NDef.1Sg\\]')
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

# -- build pairs -- #

# pair up -k and -m forms. since some -m forms are basically missing (they were probably missed by the morphological parser) we only include forms that vary within the 1sg indef.
ik4 = buildPairs(ik3)

# ...which we do here:
ik4 %<>%
  filter(!is.na(form_1) & !is.na(form_2))

# -- write -- #

# save ik4 which is the tidy merged pair thing
write_tsv(ik4, '~/Github/Racz2024/src/ik_verbs/ikes_pairs_webcorpus2.tsv')


 