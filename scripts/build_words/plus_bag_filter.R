library(tidyverse)

cselekszenek_plus = read_tsv('~/Github/Racz2024/resource/real_words/epenthetic_stems/epenthesis_reference_webcorpus2.tsv')
hotelban_plus = read_tsv('~/Github/Racz2024/resource/real_words/noun_bag.tsv')
h = h = read_tsv('~/Github/Racz2024/resource/hu_list.txt') %>% 
  pull(word)

cs_l = pull(cselekszenek_plus, lemma)
cs_l = cs_l[cs_l %in% h]
h_l = pull(hotelban_plus, lemma)
h_l = h_l[h_l %in% h]
ll = c(cs_l,h_l)

c = read_tsv('~/Github/Racz2024/resource/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')

c2 = filter(c, lemma %in% ll)
write_tsv(c2, '~/Github/Racz2024/resource/real_words/plus_bag.tsv')