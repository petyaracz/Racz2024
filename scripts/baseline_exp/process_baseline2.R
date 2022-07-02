# combine output of process_baseline w/ corpus data for plots etc, add gcm

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(magrittr)
library(lubridate)

# -- functions -- #

# take string, transcribe to single digits digraphs or back to double digit ones, return string
transcribe = function(nonce_word,direction){
  
  case_when(
    direction == 'single' ~ nonce_word %>% 
      str_replace_all(., c('cs' = 'ç', 'sz' = 'ß', 'zs' = 'Ω', 'ty' = '†', 'gy' = '©', 'ny' = '¥', 'ly' = '¬')),
    direction == 'double' ~ nonce_word %>% 
      str_replace_all(., c('ç' = 'cs', 'ß' = 'sz', 'Ω' = 'zs', '†' = 'ty', '©' = 'gy', '¥' = 'ny', '¬' = 'ly')),
    T ~ 'wrong direction, either (to) "single" or "double"'
  )
}

# take input, build gcm compatible sets, return list of two sets, test training
GCMinput = function(variation_type,lower_bound,dat1,dat2){
  if(nrow(dat1[dat1$variation == variation_type,]) == 0){
    print('You mispelt the category label')
    break
  }
  
  training = dat1 %>% 
    filter(variation == variation_type) %>%
    rename(word = base_tr) %>% 
    group_by(word,base,lemma_freq_corrected) %>% 
    summarise(
      resp1 = sum(resp1),
      resp2 = sum(resp2),
      log_odds = log( ( resp1 + 1 ) / ( resp2 + 1 ) ),
    ) %>% 
    filter(
      resp1 > lower_bound, 
      resp2 > lower_bound, 
      !is.na(log_odds)
    ) %>% 
    arrange(log_odds) %>% 
    ungroup() %>%
    mutate(
      quantile_rank = ntile(log_odds,3),
      category = case_when(
        quantile_rank == 1 ~ 'tilt_freq2',
        quantile_rank == 3 ~ 'tilt_freq1'
      )
    ) %>%
    select(word,category)
  
  target = dat2 %>% 
    filter(variation == variation_type) %>%
    mutate(word = transcribe(base, 'single')) %>% 
    select(word,base,log_odds)
  
  result = list(training,target)
  
  return(result)
}

# gcm! uses fur. expects training with word and category, word is string, category is string. expects test with word. uses future_map. returns test list with cat weights.
furGCM = function(training,test,var_s=0.3,var_p=1,distance_metric='lv'){
  
  training = training %>% droplevels 
  test = test %>% droplevels
  
  getTargetSimilarity = function(target,training){
    
    # we drop target from training in case we are cross-validating  
    training = training %>% 
      filter(
        word != target,
        !is.na(category)
      )
    
    training$target = target # this line is for tidier bookkeeping
    
    # for each row in training, calculate target ~ word-in-row distance and pairwise similarity  
    distances = training %>% 
      mutate(dist = stringdist::stringdist(word,target, method = distance_metric),
             pairwise.similarity = exp ( -dist / var_s )^var_p)
    
    # sum pairwise similarity for each category  
    category.distances = distances %>% 
      group_by(category) %>% 
      summarise(summed.pairwise.similarity = sum(pairwise.similarity))
    
    # get total similarity  
    total.similarity = distances %>% 
      summarise(sum(pairwise.similarity)) %>% 
      pull
    
    category.distances$total.similarity = total.similarity # this line is for tidier bookkeeping
    
    # get gcm weight of category  
    form.total.category.distances = category.distances %>% 
      mutate(gcm.weight = summed.pairwise.similarity / total.similarity) %>% 
      select(category, gcm.weight)
    
    # spread gcm weights. we end up with a line which is 'target w1, w2... wn'  
    ftdc.wide = form.total.category.distances %>% 
      spread(category, gcm.weight)
    
    return(ftdc.wide)
    
  }
  
  # for each target in test, get ftdc.wide using getTargetsimilarity and put into new cell, unnest. we won.  
  result = test %>% 
    mutate(similarities = furrr::future_map(word, ~ getTargetSimilarity(., training))) %>% 
    select(word, similarities) %>% 
    unnest(cols = c(similarities))
  
  return(result)
}

# -- read -- #

s = read_tsv('resource/real_words/all_pairs_webcorpus2.tsv')
d2 = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')
v = read_tsv('resource/real_words/verb_bag.tsv')
h = read_tsv('resource/hu_list.txt')

# -- wrangle s -- #

# s %<>%
#   filter(!stem %in%
#            c('wales','bayern','chanel','james','carter','warner','schubert','carmen','aspen','wagner','gallen','stadler','luzern','karen','rogers','assen','albert','luther','angel','costes','tanger','rockets','sacher','aden','wawel','russell','torres','adler','mayer','basel','mater','novemb','mahler','szandek','ranked','anger','posen','wattens','quest','angers','landler','gallery','wanted','smplayer','jatek','vámbéry','roses','trumpék','athens','caen','nantes','cannes','moyes','holmes','forbes','jones','haren','barney','arnyek','charles','romney','annecy','barnes','matthews','andrews','kastely','poen','tanyer','wolves','notes','athen','kroes','crowley','rhodes','carsely','barnsley','huxley','ugyer','foles','mosley','marley','matthew','bander','chartres','joffrey','rocher','sagres','arles','sallent','barkley','corey','chocen','crawley','honey','xanten','apey','carles','corel','jaén','brawnék','bullet','prades','cutter','fossey','jocelyn','lovely','bowles','knowles','mladen','downey','gaffney','scholem','smokey','tobey','appleby','fásyék','oek','onew','orkney','rather','rocket','scholes','szayel','zsoter','ales','allel','armey','bubbles','closet','crosbyék','father','goddess','gottsched','luffyék','orthez','psylander','rfotel','rosset','ryanék','valéry','aner','bobbyék','chanter','bushey','curves','djanet','dorsey','fawkes','gather','glyptothek','hotelb','hotell','kmplayer','later','markets','paffett','pages','pardew','povey','salles','sarcey','storey','states','tholey','thommey','valley','watney','access','acer','achen','aely','agency','agent','alves','ashley','bowen','brother','brothers','bruges','buffett','busher','butler','butlers','cancel','cassel','channel','chapel','chapter','charmed','comedy','coventry','dramedy','dsungel','farel','farrell','flores','flowers','fowler','games','gates','godfrey','graves','gyogyszer','haley','halley','haven','hotels','janet','joel','kaspersky','lakers','maker','masters','modszer','mommsen','mores','moresby','novel','novell','owen','owens','panthers','panzers','planet','poetry','powell','process','ramsey','randers','ranger','ronneby','russel','sawyer','slasher','stacey','sussex','surrey','szoveg','thunder','thomsen','tower','tunnel','valley','vranjes','warren','yates','valles','druten','mather','andes','stander','mozes','hades','lasker','ganden','rosék','varley','wangen','mccartney','opengl','cruces','ahet','pales','oneg','lomex','hamer','smoker','survey','uther','sales','travel','rachel','open','bayer','arres','kardelj','bokeh','candes','cateh','affer','taeg','punter','kayes','flower','abbey','tarbes','blues','naples','sabres','norveg','buster','codek','hayley','thumper','hunter','wrapper')
#   )

s2 = s %>% 
  mutate(
    keep = ifelse((!(stem %in% h$word) & variation == 'hotelban/hotelben'),F,T),
    type = 'real word corpus freq',
    derivational = str_replace(derivational, '^','-'),
    variation2 = ifelse(variation == 'lakom/lakok', 'lakok/lakom', variation),
    variation = variation2
  ) %>% 
  rename(variant1 = form_1,variant2 = form_2,resp1 = freq_1,resp2 = freq_2) %>%
  filter(keep) %>% 
  select(base,base_tr,variation,type,log_odds,derivational,nsyl,vowel,suffix,lemma_freq_corrected,variant1,variant2,resp1,resp2)

# -- wrangle v -- #

cselekszik_var =
  s %>% 
  filter(
    variation == 'cselekszenek/cselekednek',
    xpostag == '[/V][Prs.NDef.3Pl]'
    ) %>%
  mutate(
    f1 = str_replace(form_1, '.n.k$', 'ik'),
    f2 = str_replace(form_2, 'n.k$', 'ik')
  ) %>% 
  select(stem,f1,f2) %>% 
  pivot_longer(-stem) %>% 
  pull(value)

cselekedik_training = v %>% 
  filter(
    str_detect(lemma, 'ik$'),
    !lemma %in% cselekszik_var,
    str_detect(lemma, '(dzik$|llik$)', negate = T)
    ) %>% 
  mutate(
    word = transcribe(lemma, 'single'),
    category = case_when(
      str_detect(word, '[^aáeéiíoóöőuúüű]{2}ik$') ~ 'ccik',
      str_detect(word, '[^aáeéiíoóöőuúüű][aáeéiíoóöőuúüű][^aáeéiíoóöőuúüű]ik$') ~ 'cvcik'
    )
  ) %>% 
  filter(!is.na(category)) %>% 
  select(word,category)

# -- wrangle d2 -- #

# -- > gcm

## nouns: nonce vs real
res1 = GCMinput('hotelban/hotelben',10,s2,d2)
ntraining = res1[[1]]
ntest = res1[[2]]
n_gcm_real = furGCM(ntraining,ntest, var_s = .9, var_p = 1, distance_metric = 'lv')
nres = inner_join(ntest,n_gcm_real, by = 'word')

## eszek: nonce vs real
res2 = GCMinput('lakok/lakom',10,s2,d2)
lakoktraining = res2[[1]]
lakoktest = res2[[2]]
n_gcm_real = furGCM(lakoktraining,lakoktest, var_s = .9, var_p = 1, distance_metric = 'lv')
lakokres = inner_join(lakoktest,n_gcm_real, by = 'word')

## cselekedik: nonce vs real
res3 = GCMinput('cselekszenek/cselekednek',10,s2,d2)
cselekediktest = res3[[2]]
cselekediktest %<>% mutate(
  word = unlist(str_extract(word, '^.*(?= /)'))
) 
# training with stiff cvc/cc verbs
n_gcm_real = furGCM(cselekedik_training,cselekediktest, var_s = .7, var_p = 1, distance_metric = 'lv')
cselekedikres2 = inner_join(cselekediktest,n_gcm_real, by = 'word')
cselekedikres2$tilt_freq1 = cselekedikres2$ccik # column name mopup

## merge back
d2 = bind_rows(nres,lakokres,cselekedikres2) %>% 
  rename(
    cat1_weight = tilt_freq1
  ) %>% 
  select(base,cat1_weight) %>% 
  right_join(d2, by = 'base')

# -- > merge everything

d3 = bind_rows(d2,s2) %>% 
  mutate(
    stem_vowel = ifelse(variation == 'hotelban/hotelben', str_extract(base_tr, '[aáeéiíoóöőuúüű]'), NA),
    long_cluster = ifelse(variation == 'hotelban/hotelben', nchar(str_extract(base_tr, '(?<=[aáeéiíoóöőuúüű])[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])')) > 1, NA),
    consonants = ifelse(variation == 'cselekszenek/cselekednek', glue("{str_extract(base_tr, '.(?=ik /)')}/{str_extract(base_tr, '.(?=ik$)')}"), NA)
  )  

write_tsv(d3, 'exp_data/baseline/baseline_tidy_proc_with_corpus_and_gcm.tsv')  

ntraining$variation = 'hotelban/hotelben'
lakoktraining$variation = 'lakok/lakom'
cselekedik_training$variation = 'cselekszenek/cselekednek'
bind_rows(ntraining,lakoktraining,cselekedik_training) %>% 
  mutate(szo = transcribe(word, 'double')) %>% 
  write_tsv('exp_data/baseline/baseline_gcm_ref_sets.tsv')
