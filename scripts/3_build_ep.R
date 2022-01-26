## build epenthetic verb stem list from webcorpus2 ##

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

# All epenthetic verbs have -CCik forms, but not all -CCik verbs are epenthetic. There are no formal grounds to separate the sets, because, for some verbs, the CC and CVC forms mean the same thing and are variants of each other (áramolna, áramlana), for others, there is a difference in meaning (hajolna, hajlana). In addition, for some variable forms, the morphological parser used to create the webcorpus assigned separate stems for variant forms (bomol for bomolnak, bomlik for bomlanak). I extracted all verb lemmata ending in non-geminate CCik from the frequency list. I hand-filtered these to remove verbs that are (i) defective (ii) have a meaning-difference and hand-adjusted the CVC forms. Then I used this list to create all possible lemma forms for the verbs: CCik, CVCik, CC, CVC (áramlik, áramolik, áraml, áramol). Some of these will never exist in isolation, but they are possibly assigned as lemmata for the variable forms by the morphological parser. The frequency list was filtered using this second list, and the results hand-cleaned. Then I paired up CC and CVC variants per stem and calculated log odds. The final list is slightly awkward in that the unique lemma identifiers (the stem column) are not exhaustive in a larger search (many words have the same "stem") and the lemmata are all over the place even in the dataset, because they reflect the parser's decisions over the crawled rather than theory/intuition on what forms belong together.

# -- find -- #

# some nice variable postags

postags = c("[/V][Prs.NDef.3Pl]","[/V][Inf]","[/V][Pst.NDef.3Pl]","[/V][Cond.NDef.3Sg]","[/V][Prs.NDef.2Pl]")

# # pre-compile list, no other way

# ik = cfull %>%
#   filter(
#     hunspell,
#     xpostag == '[/V][Inf]',
#     str_detect(lemma, '[rtpsdfghjklzxcvbnmy](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])ik$')
#   ) %>%
#   distinct(lemma)
# 
# ik %>%
#   arrange(lemma) %>%
#   write_tsv('notes/ik.txt')

# removed a few iffy ones, added the CVC form
hanglemmata = read_tsv('src/epenthetic_stems/stemlist_from_webcorpus_cleared.tsv')

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

# -- build pairs -- #

hang4 = buildPairs(hang3)

# -- write -- #

write_tsv(hang4, 'src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
  
# -- quality control -- #

unique(ik_old$exponent)


# -- comparison set -- #

verbs = cfull %>% 
    filter(
      hunspell,
      xpostag == '[/V][Inf]',
      str_detect(lemma, 'ik$')
    ) %>%
    distinct(lemma,corpus_size,lemma_freq)

verbs %<>% 
  filter(!(lemma %in% c(iklemmata2$lemma, 'antik','piknik','szik'))) %>% 
  mutate(
    category = ifelse(
      str_detect(lemma, '[aáeéiíoóöőuúüű](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])ik$'), 'cvc', 'cc'
    ) 
  )

write_tsv(verbs, 'src/epenthetic_stems/epenthesis_reference_webcorpus2.tsv')
