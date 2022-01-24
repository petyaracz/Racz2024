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

# -- read -- #

# hu = read_tsv('~/Github/Racz2024/src/hu_list.txt')
# c = read_tsv('~/Documents/Webcorpus2/frequency_list/data/freqlists2/webcorpus2_freqlist_hu_with_lemmafreq_hu_list_filt.tsv.gz')
cfull = read_tsv('~/Documents/Webcorpus2/frequency_list/data/freqlists2/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')

# -- find -- #

# variable postags

postags = c("[/V][Prs.NDef.3Pl]","[/V][Inf]","[/V][Pst.NDef.3Pl]","[/V][Cond.NDef.3Sg]","[/V][Prs.NDef.2Pl]")

# pre-compile list, no other way

ik = cfull %>% 
  filter(
    hunspell,
    xpostag == '[/V][Inf]',
    str_detect(lemma, '[rtpsdfghjklzxcvbnm](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])ik$')
  ) %>% 
  distinct(lemma)

ik %>% 
  arrange(lemma) %>% 
  write_tsv('notes/ik.txt')

# removed a few iffy ones, added the CVC form

iklemmata = read_tsv('src/epenthetic_stems/stemlist_from_webcorpus_cleared.tsv')

iklemmata %<>% 
  mutate(
    cvcik = str_replace(na, 'n[ae]$', 'ik'),
    cc = str_replace(ccik, 'ik$', ''),
    cvc = str_replace(cvcik, 'ik$', ''),
    stem = str_replace(cvc, '[aáeéiíoóöőuúüű](sz|zs|ny|ty|gy|[rtpsdfghjklzxcvbnm])$', '')
  ) %>%
  select(-na) %>% 
  pivot_longer(-stem, names_to = 'stem_type', values_to = 'lemma')

ik = inner_join(iklemmata,cfull, by = "lemma")

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
  arrange(stem,xpostag,type) %>% 
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
  select(cvc_form,cvc_freq,stem,xpostag,stem_freq,corpus_size)

ccik_v = ik2 %>% 
  filter(
    type == 'CC'
  ) %>% 
  rename('cc_form' = form, 'cc_freq' = freq) %>% 
  select(cc_form,cc_freq,stem,xpostag,stem_freq,corpus_size)

ik3 = full_join(cvcik_v,ccik_v, by = c("stem", "stem_freq", "xpostag", "corpus_size")) %>%
  arrange(stem,xpostag) %>% 
  select(stem,cvc_form,cc_form,cvc_freq,cc_freq,stem_freq,corpus_size,xpostag)

# need to build a lemma placeholder.

# -- quality control -- #



