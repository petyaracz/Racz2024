## build -ik list from webcorpus2 ##

# -- header -- #

setwd('~/Github/Racz2024/')

set.seed(1337)

library(tidyverse)
library(ggthemes)
library(furrr)
library(broom)

# -- read -- #

hu = read_tsv('~/Github/Racz2024/src/hu_list.txt')
c = read_tsv('~/Documents/Webcorpus2/frequency_list/data/freqlists2/webcorpus2_freqlist_hu_with_lemmafreq_hu_list_filt.tsv.gz')
cfull = read_tsv('~/Documents/Webcorpus2/frequency_list/data/freqlists2/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')
ik_old = read_csv('src/ik_verbs/ikes_pairs_dataset.csv')

# -- list builder -- #

# 1sg indef ik forms can have the exponent -k or -m. For some, though not all, -k variants, there is a separate lemma for the -k form and the -m form. This means you can't filter the list based on lemma. I drew all the -m 1sg indef forms with an -ik lemma, took the form w/o the variable final consonant (m/k) and then used these string beginnings to filter the entire corpus and create pairs. The resulting list was then filtered based on the spellchecker word list. The spellchecker word list undershoots and excludes many extant forms, but the unfiltered list has a lot of rubbish in it. I suspect that some -m variants are incorrectly coded as definite forms by the pos tagger, but I have no way of verifying that. This setup leaves some ill-defined lemmata, so I remove any with a frequency under 10. This removes ten pairs.

"
s %>% 
  filter(
    str_detect(lemma, '.*ik$'),
    str_detect(xpostag, '\\[\\/V\\]')
  )
"

# take cfull, grab 1sg indef for V lemmata ending in ik
ik1 = cfull %>% 
  filter(
    str_detect(lemma, 'ik$'),
    str_detect(xpostag, '\\[\\/V\\]\\[Prs.NDef.1Sg\\]')
  )

# take 1sg indef w/o variable ending (esze[km])
beginnings = ik1 %>% 
  pull(form) %>% 
  str_replace('.$','')

# trawl cfull for these 'beginnings' note: this misses out on verbs that are given with non-ik lemmata and so don't make it in ik1
ik2 = cfull %>% 
  mutate(
    beginning = str_replace(form, '.$', '')
  ) %>% 
  filter(
    beginning %in% beginnings,
    str_detect(xpostag, '\\[\\/V\\]\\[Prs.NDef.1Sg\\]')
  )

# let's see
# ik2 %>% filter(beginning == 'gondolkodo') %>% View

# -m variants. we keep lemma tallies separate because we have "gondolkoz" "gondolkozik" lemma pairs
ikm = ik2 %>% 
  filter(str_detect(form, 'm$')) %>% 
  select(form,lemma,freq,lemma_freq,corpus_size,beginning) %>% 
  rename('m_form' = form, 'm_freq' = freq, 'm_lemma' = lemma, 'm_lemma_freq' = lemma_freq)

# -k variants.
ikk = ik2 %>% 
  filter(str_detect(form, 'k$')) %>% 
  select(form,lemma,freq,lemma_freq,corpus_size,beginning) %>% 
  rename('k_form' = form, 'k_freq' = freq, 'k_lemma' = lemma, 'k_lemma_freq' = lemma_freq)

# full join, add 0 as freq for variants that have no pairs. calculate log odds. get rid of lemmata w/ freq<10, because those are very likely to be rubbish. alternative would be filtering using spellcheck list, that's too restrictive. for the time being.
ik3 = ikm %>% 
  full_join(ikk, by = c("corpus_size", "beginning")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    lemma_freq = m_lemma_freq + k_lemma_freq,
    m_k_odds =  ( m_freq + 1 ) / ( k_freq + 1 ),
    m_k_log_odds = log(m_k_odds)
    ) %>% 
  filter(
    m_lemma %in% hu$word | k_lemma %in% hu$word,
    lemma_freq > 10
  )

# -- quality control -- #

# longer words should favour one variant
ik3 %>% 
  mutate(
    length = nchar(beginning)
  ) %>% 
  ggplot(aes(length,m_k_log_odds)) +
  geom_point() +
  theme_few() +
  geom_smooth()

# frequency shouldn't do anything
ik3 %>% 
  ggplot(aes(log10(lemma_freq),m_k_log_odds)) +
  geom_point() +
  theme_few() +
  geom_smooth()

# compare to Acta Linguistica paper I wrote years ago
ik4 = ik_old %>% 
  mutate(
    beginning = str_replace(indef_um, '.$', '')
  ) %>% 
  rename('m_k_log_odds_w1' = log_odds_m) %>% 
  select(beginning,m_k_log_odds_w1) %>% 
  right_join(ik3, ., by = "beginning")

ik4 %>% 
  ggplot(aes(m_k_log_odds,m_k_log_odds_w1,label = beginning)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm')

ik4 %>% 
  ggplot(aes(m_k_log_odds,m_k_log_odds_w1,label = beginning)) +
  geom_label() +
  theme_bw()
ggsave('vis/ik_comp.png', width = 12, height = 12)

tidy(with(ik4, cor.test(m_k_log_odds,m_k_log_odds_w1)))

# ik4 %>% 
  # filter(beginning == 'sakkozo')

# -- write -- #

# save ik3 which is the tidy merged pair thing
write_tsv(ik3, '~/Github/Racz2024/src/ik_verbs/ikes_pairs_webcorpus2.tsv')


 