## build epenthetic verb stem list from webcorpus2 ##

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(glue)
library(magrittr)

library(ggthemes)
library(furrr)
library(broom)

# All epenthetic verbs have -CCik forms, but not all -CCik verbs are epenthetic. There are no formal grounds to separate the sets, because, for some verbs, the CC and CVC forms mean the same thing and are variants of each other (áramolna, áramlana), for others, there is a difference in meaning (hajolna, hajlana). In addition, for some variable forms, the morphological parser used to create the webcorpus assigned separate stems for variant forms (bomol for bomolnak, bomlik for bomlanak). I extracted all verb lemmata ending in non-geminate CCik from the frequency list. I hand-filtered these to remove verbs that are (i) defective (ii) have a meaning-difference and hand-adjusted the CVC forms. Then I used this list to create all possible lemma forms for the verbs: CCik, CVCik, CC, CVC (áramlik, áramolik, áraml, áramol). Some of these will never exist in isolation, but they are possibly assigned as lemmata for the variable forms by the morphological parser. The frequency list was filtered using this second list, and the results hand-cleaned. Then I paired up CC and CVC variants per stem and calculated log odds. The final list is slightly awkward in that the unique lemma identifiers (the stem column) are not exhaustive in a larger search (many words have the same "stem") and the lemmata are all over the place even in the dataset, because they reflect the parser's decisions over the crawled rather than theory/intuition on what forms belong together.

# -- read -- #

cfull = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')
ik_old = read_csv('src/epenthetic_stems/formatted_filtered_hits_pairs_jan.txt')

# -- find -- #

# variable postags

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

iklemmata = read_tsv('src/epenthetic_stems/stemlist_from_webcorpus_cleared.tsv')

iklemmata %<>% 
  mutate(
    cvcik = str_replace(na, 'n[ae]$', 'ik'),
    cc = str_replace(ccik, 'ik$', ''),
    cvc = str_replace(cvcik, 'ik$', ''),
    stem = str_replace(cvc, '[aáeéiíoóöőuúüű](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])$', '')
  ) %>%
  select(-na)
  
iklemmata2 = iklemmata %>% 
  pivot_longer(-stem, names_to = 'stem_type', values_to = 'lemma')

ik = inner_join(iklemmata2,cfull, by = "lemma")

ik2 = ik %>% 
  group_by(stem) %>% 
  mutate(
    stem_freq = sum(lemma_freq)
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

ik2 %<>% 
  filter(
    !(form %in% c('aldni','gyanakdni','nyugdni','telepdni','veszekdni','vetekdni'))
  )

ik2 %<>% 
  select(stem,lemma,form,xpostag,freq,type,stem_freq,corpus_size) %>% 
  group_by(stem,xpostag,type) %>%
  slice(1)
  
ik2 %<>% 
  group_by(stem,xpostag) %>% 
  filter(n() == 2)

cvcik_v = ik2 %>% 
  filter(
    type == 'CVC'
    ) %>% 
  rename('cvc_form' = form, 'cvc_freq' = freq) %>% 
  select(cvc_form,cvc_freq,stem,xpostag,stem_freq,corpus_size,lemma)

ccik_v = ik2 %>% 
  filter(
    type == 'CC'
  ) %>% 
  rename('cc_form' = form, 'cc_freq' = freq) %>% 
  select(cc_form,cc_freq,stem,xpostag,stem_freq,corpus_size)

ik3 = full_join(cvcik_v,ccik_v, by = c("stem", "stem_freq", "xpostag", "corpus_size")) %>%
  ungroup() %>% 
  arrange(stem,xpostag) %>% 
  select(stem,lemma,cvc_form,cc_form,cvc_freq,cc_freq,stem_freq,corpus_size,xpostag)

ik3 %<>%
  mutate(
    cvc_cc_odds = (cvc_freq + 1 ) / (cc_freq + 1 ),
    cvc_cc_log_odds = log(cvc_cc_odds)
  )

write_tsv(ik3, 'src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
  
# -- quality control -- #

unique(ik_old$exponent)

ik4 = ik_old %>% 
  rename('cvc_form' = v_form, 'log_odds_w1' = log_odds) %>% 
  select(stem,cvc_form,log_odds_w1) %>% 
  inner_join(ik3,., by = c("stem", "cvc_form"))

# yesss
with(ik4, cor.test(cvc_cc_log_odds,log_odds_w1))
ggplot(ik4, aes(cvc_cc_log_odds,log_odds_w1,label = cc_form)) +
  geom_text() +
  theme_few()

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
