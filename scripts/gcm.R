setwd('~/Github/Racz2024/')

library(tidyverse)
library(stringdist)

# -- read -- #

ep = read_tsv('src/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
er = read_tsv('src/epenthetic_stems/epenthesis_reference_webcorpus2.tsv')

# -- functions -- #

oldGCM = function(test_forms,category_A,category_B,all_forms,var_s,var_p){
  
  vector_sim_cat_A = as.numeric(NULL)
  vector_sim_cat_B = as.numeric(NULL)
  
  for (i in 1:length(test_forms)){
    form1 = test_forms[i]  
    
    vector_pairwise_sims_cat_A = as.numeric(NULL)
    
    for (ii in 1:length(category_A)){
      form2 = category_A[ii]
      dist = stringdist(form1,form2, method = 'lv')
      pairwise_sim = exp ( - dist / var_s )^var_p
      vector_pairwise_sims_cat_A[ii] = pairwise_sim
    }
    
    sum_pairwise_sims_cat_A = sum(vector_pairwise_sims_cat_A)
    
    vector_pairwise_sims_cat_B = as.numeric(NULL)
    
    for (ii in 1:length(category_B)){
      form2 = category_B[ii]
      dist = stringdist(form1,form2, method = 'lv')
      pairwise_sim = exp ( - dist / var_s )^var_p
      vector_pairwise_sims_cat_B[ii] = pairwise_sim
    }
    
    sum_pairwise_sims_cat_B = sum(vector_pairwise_sims_cat_B)
    
    vector_pairwise_sims_all_forms = as.numeric(NULL)
    
    for (ii in 1:length(all_forms)){
      form2 = all_forms[ii]
      dist = stringdist(form1,form2, method = 'lv')
      pairwise_sim = exp ( - dist / var_s )^var_p
      vector_pairwise_sims_all_forms[ii] = pairwise_sim
    }
    
    sum_pairwise_sims_all_forms = sum(vector_pairwise_sims_all_forms)
    
    sim_cat_A = sum_pairwise_sims_cat_A / sum_pairwise_sims_all_forms
    sim_cat_B = sum_pairwise_sims_cat_B / sum_pairwise_sims_all_forms
    
    vector_sim_cat_A[i] <- sim_cat_A
    vector_sim_cat_B[i] <- sim_cat_B
    
  }
  
  output_thing <- cbind(test_forms,vector_sim_cat_A,vector_sim_cat_B) %>% data.frame %>% rename(disc = test_forms, weight_A = vector_sim_cat_A, weight_B = vector_sim_cat_B) 
  
  return(output_thing)
}

# -- wrangle -- #

ep_verbs = ep %>% 
  select(stem,form_1) %>% 
  mutate(
    ending = form_1 %>% 
      str_extract(glue('(?<={stem}).*$')) %>% 
      str_extract('^[^aáeéiíoóöőuúüű]*'),
    cclemma = glue('{stem}{ending}ik')
  ) %>% 
  distinct(stem,cclemma) %>% 
  filter(str_detect(cclemma, '(dnik$|ztik$)', negate = T))

target = pull(ep_verbs,cclemma)
cvc_cat = er %>% filter(category == 'cvc') %>% pull(lemma)
cc_cat = er %>% filter(category == 'cc') %>% pull(lemma)
all_forms = c(cvc_cat,cc_cat)

# -- test code -- #

old_res = oldGCM(target,cvc_cat,cc_cat,all_forms,0.3,1)

# -- comp -- #

ep %<>% 
  left_join(ep_verbs) %>% 
  rename('disc' = cclemma) %>% 
  left_join(old_res)

ep %>% 
  mutate(
    weight_A = as.double(weight_A)
  ) %>% 
  ggplot(aes(weight_A,log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ xpostag) +
  theme_bw()

ep %>% 
  mutate(
    weight_A = as.double(weight_A)
  ) %>% 
  group_by(xpostag) %>% 
  nest() %>% 
  mutate(
    cor = map(data, ~ broom::tidy(cor.test(.$log_odds, .$weight_A)))
  ) %>% 
  select(xpostag,cor) %>% 
  unnest(cols = c('cor'))
