######################################################
# stimulus builder: 2. tests comparing webcorpus 1 and 2
# @raczpetya
######################################################


# these are not really tests. ha. they compare results to previous runs with the old webcorpus (in the case of ik and epenthetic stems, these were published Rácz Acta Linguistica and Rácz Rebrus Törkenczy CLLT. in the case of vh I just made a quick search here.)

library(ggthemes) # welcome to the package loader easter egg hunt!

########################################################
## ik
########################################################

ik = read_tsv('resource/ik_verbs/ikes_pairs_webcorpus2.tsv')
ik_old = read_csv('resource/ik_verbs/ikes_pairs_webcorpus1.csv')

ik = ik_old %>% 
  mutate(
    stem = str_replace(indef_um, '.$', '')
  ) %>% 
  rename('log_odds_w1' = log_odds_m) %>% 
  select(stem,log_odds_w1) %>% 
  inner_join(ik, ., by = "stem")

ik %>% 
  ggplot(aes(log_odds,log_odds_w1,label = stem)) +
  geom_label() +
  theme_bw() +
  geom_smooth(method = 'lm')

with(ik, cor.test(log_odds,log_odds_w1))

########################################################
## vh
########################################################

# http://szotar.mokk.bme.hu/szoszablya/searchq.php?t=type&l=100&q=lemma%7E%5E%5B%5Ea%E1e%E9i%EDo%F3%F6%F5u%FA%FC%FB%5D%2B%5Ba%E1o%F3u%FA%5D%5B%5Ea%E1e%E9i%EDo%F3%F6%F5u%FA%FC%FB%5D%2B%5Be%E9%5D%5B%5Ea%E1e%E9i%EDo%F3%F6%F5u%FA%FC%FB%5D%2B%24+analysis%7E%28NOUN%7CADJ%29%3CCAS%3C%28ILL%7CINE%7CADE%7CDAT%7CSBL%7CADE%29%3E%3E
# lemma~^[^aáeéiíoóöőuúüű]+[aáoóuú][^aáeéiíoóöőuúüű]+[eé][^aáeéiíoóöőuúüű]+$ analysis~(NOUN|ADJ)<CAS<(ILL|INE|ADE|DAT|SBL|ADE)>>
vh = read_tsv('resource/front_harmony/fh_pairs_webcorpus2.tsv')
vh_old = read_delim('resource/front_harmony/fh_pairs_webcorpus1.csv', delim = ';')

vh_old %<>% 
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
vh_olda = vh_old %>% 
  filter(variant == 'a') %>% 
  select(word,freq,lemma,lemmafreq,analysis) %>% 
  rename('a_form' = word, 'a_freq' = freq)

vh_olde = vh_old %>% 
  filter(variant == 'e') %>% 
  select(word,freq,lemma,lemmafreq,analysis) %>% 
  rename('e_form' = word, 'e_freq' = freq) 

# merge variant sets, replace na with 0, tidy up tags for merge, remove lemmata not in spellcheck.
vh_old2 = full_join(vh_olda,vh_olde, by = c("lemma", "lemmafreq", "analysis")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(
    a_e_odds_w1 =  ( a_freq + 1 ) / ( e_freq + 1 ),
    a_e_log_odds_w1 = log(a_e_odds_w1),
    tag = str_extract(analysis, '...(?=>>$)') 
  ) %>% 
  filter(
    lemma %in% h
  )

vh_old3 = vh %>% 
  mutate(
    lemma = stem,
    tag = str_extract(xpostag, '(?<=\\]\\[).*(?=\\]$)') %>% 
      str_to_upper() %>% 
      str_replace('SUBL','SBL')
  ) %>% 
  select(lemma,tag,log_odds) %>% 
  right_join(vh_old2,., by = c("lemma", "tag"))

# yes this is good
with(vh_old3, cor.test(log_odds,a_e_log_odds_w1))

# excellent
vh_old3 %>% 
  ggplot(aes(log_odds,a_e_log_odds_w1,label = lemma)) +
  geom_text() +
  theme_few()

########################################################
## epenthetic stems
########################################################

hang = read_tsv('resource/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
hang_old = read_csv('resource/epenthetic_stems/epenthesis_pairs_webcorpus1.txt')


hang2 = hang_old %>% 
  rename('form_2' = v_form, 'log_odds_w1' = log_odds) %>% 
  select(stem,form_2,log_odds_w1) %>% 
  inner_join(hang,., by = c("stem", "form_2"))

# good enough
with(hang2, cor.test(log_odds,log_odds_w1))

ggplot(hang2, aes(log_odds,log_odds_w1,label = form_1)) +
  geom_text() +
  theme_few()
