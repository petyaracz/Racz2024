## data

c = read_tsv('src/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')
h = read_tsv('src/hu_list.txt') %>% 
  pull(word)

hik = str_replace(h, 'ik$', '')

## functions

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
