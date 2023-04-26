# helper files for the baseline learner paper
# pracz

# -- header -- #

# setwd('~/Github/Racz2024') # the Rmd does this

set.seed(1989)

library(tidyverse)
library(magrittr)
library(glue)

library(patchwork)
library(gghalves)
library(ggthemes)

library(lme4)
library(knitr)

# -- data -- #

lakok = read_tsv('~/Github/Racz2024/resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
cselekszenek = read_tsv('~/Github/Racz2024/resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
hotelban = read_tsv('~/Github/Racz2024/resource/real_words/front_harmony/fh_pairs_webcorpus2.tsv')
cselekszenek_plus = read_tsv('~/Github/Racz2024/resource/real_words/epenthetic_stems/epenthesis_reference_webcorpus2.tsv')
hotelban_plus = read_tsv('~/Github/Racz2024/resource/real_words/noun_bag.tsv')
plus = read_tsv('~/Github/Racz2024/resource/real_words/plus_bag.tsv')

h = read_tsv('~/Github/Racz2024/resource/hu_list.txt') %>% 
  pull(word)

v = read_tsv('~/Github/Racz2024/resource/real_words/verb_bag.tsv')
b = read_tsv('~/Github/Racz2024/exp_data/baseline/baseline_tidy_proc.tsv')

# -- functions -- #

# GCM code. see there for comments
source('~/Github/Racz2024/models/learner/baseline/purrrgcm.R')
# code to transcribe Hungarian digraphs to single char. see there for comments
source('~/Github/Racz2024/models/learner/baseline/transcribe.R')

# take real lang dat, postag list, keep more frequent form per each postag.
filterPlus = function(dat,postag_list){
  dat %>% 
    select(lemma,category) %>% 
    left_join(plus) %>% 
    filter(xpostag %in% postag_list) %>% 
    arrange(lemma,xpostag,-freq) %>%
    group_by(lemma,xpostag) %>% 
    slice(1) %>% 
    ungroup()
}

# take dat, two variables, return nice cloud plot
cloudPlot = function(dat,a,b){
  dat %>% 
  filter(!is.na({{a}})) %>% 
  ggplot(aes({{a}},{{b}})) +
  geom_half_violin(side = 'r', alpha = .5) +
  geom_half_boxplot(side = 'r', width = .1) +
  geom_half_point(side = 'l', width = .5, size = .5, range_scale = .5) +
  xlab('derivational suffix') +
  ylab('log odds') +
  theme_bw() +
  coord_flip()
}

# take list of gcms, test file w/ log odds from real data, name of column, return gcm with highest r2
getBestGCM = function(gcms, test, colname){
  map_dfr(gcms, ~ 
  left_join(., test, by = 'word') %>% 
    summarise(r2 = cor({{colname}}, log_odds))
) %>% 
  mutate(n = 1:n()) %>% 
  slice_max(r2)
}

# take training data, category names, test data, write an mgl in file that has each form with token frequency
buildMglInWeighted = function(train,cat_name_1,cat_name_2,test){
  
  # we need a header or else everything breaks
  header_1 = c(
            'Phonological Learner File',
            'P Rácz',
            'Hungarian',
            'Tue Apr 13 2023',
            'Tue Apr 13 2023',
            'This data set was built in baseline_learner.Rmd',
            'Morphological categories:'
            )
  header_2 = glue('\t{cat_name_1}\t{cat_name_2}')
  header_3 = 'Input forms:'

  # here come the input forms for form1 and form2 in the mgl-prescribed format
  mgl_dat_1 = train %>% 
    select(base, base_tr, form_1, freq_1) %>% 
    rename('form' = form_1, 'freq' = freq_1) %>% 
    mutate(
      form_tr = transcribe(form, 'single'),
      cat = cat_name_1
      )

  mgl_dat_2 = train %>% 
    select(base, base_tr, form_2, freq_2) %>% 
    rename('form' = form_2, 'freq' = freq_2) %>% 
    mutate(
      form_tr = transcribe(form, 'single'),
      cat = cat_name_1
           )

  body = bind_rows(mgl_dat_1,mgl_dat_2) %>% 
    arrange(-freq) %>% 
    select(base_tr,form_tr,freq,base,form,cat) %>% 
    mutate(row = paste(base_tr,form_tr,freq,base,form,cat, sep = '\t')) %>% 
  pull(row)
  
  # and then come the test forms
  test_forms = test %>% 
    pull(base_tr)
  
  # we glue everything together
  c(header_1,header_2,header_3,body,'Test forms:',test_forms,'end')
}

# create a version of the gcm in file that's like the form is either form1 or form2
buildMglInBinary = function(train,cat_name_1,cat_name_2,test,cselekszenek = F){
  
  # header obligatory
  header_1 = c(
    'Phonological Learner File',
    'P Rácz',
    'Hungarian',
    'Tue Apr 13 2023',
    'Tue Apr 13 2023',
    'This data set was built in baseline_learner.Rmd',
    'Morphological categories:'
  )
  header_2 = glue('\t{cat_name_1}\t{cat_name_2}')
  header_3 = 'Input forms:'
  
  # set up either/or forms in mgl format
  body = train %>% 
    arrange(-lemma_freq_corrected) %>% 
    select(base_tr,form_tr,lemma_freq_corrected,base,form,category) %>% 
    mutate(row = paste(base_tr,form_tr,lemma_freq_corrected,base,form,category, sep = '\t')) %>% 
    pull(row)
  
  # add test forms
  test_forms = test %>% 
    pull(base_tr)
  
  # cselekszenek needs the illicit sequences list
  if(cselekszenek==T){
    c(header_1,header_2,header_3,body,'Test forms:',test_forms,'Illicit sequences:',illicit_sequences,'end')
  } else {
    c(header_1,header_2,header_3,body,'Test forms:',test_forms,'end')
  }
  
}

# csel has specific postags that we need to nest by. because separate mgl for each!
nestCselekszenek = function(dat){
  dat %>% 
    filter(!is.na(xpostag)) %>% 
    mutate(
      xpostag = case_when(
        xpostag == "[/V][Cond.NDef.3Sg]" ~ 'na',
        xpostag == "[/V][Prs.NDef.1Pl]" ~ 'unk',
        xpostag == "[/V][Prs.NDef.2Pl]" ~ 'tok',
        xpostag == "[/V][Prs.NDef.3Pl]" ~ 'nak',
        xpostag == "[/V][Inf]" ~ 'ni',
        xpostag == "[/V][Pst.NDef.3Pl]" ~ 'tak' 
      )
    ) %>% 
    group_by(xpostag) %>% 
    nest()
}

# same here.
nestHotelban = function(dat){
  dat %>% 
    filter(!is.na(xpostag)) %>% 
    mutate(
      xpostag = str_extract(xpostag, '(Subl|Ill|Ine|Ade|Dat)(?=\\]$)')
    ) %>% 
    group_by(xpostag) %>% 
    nest()
}

# find names of sum files for vectorised read tsv return master w/ labels
getMGLmaster = function(my_dir){
  main_path = '~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/'
  paths = glue('{main_path}{my_dir}') %>% 
    list.files() %>% 
    str_extract('.*sum$') %>% 
    na.omit()
  tibble(
    file_name = paths,
    path = glue('{main_path}{my_dir}/{file_name}')
  ) %>% 
    rowwise() %>% 
    mutate(
      rules = list(read_tsv(path)),
      training_type = str_extract(file_name, '[0-9]')
    ) %>% 
    ungroup() %>% 
    select(training_type,rules) %>% 
    unnest(cols = 'rules')
  
}

# take data file, test file, name of variation, spit out rules for the two variants
getMGLrules = function(dat,test,my_variation){
  
  # relevant variants
  variants = b %>% 
    filter(variation == my_variation) %>% 
    select(base_tr,variant1,variant2) %>% 
    pivot_longer(-base_tr, names_to = 'variant_number', values_to = 'variant') %>% 
    mutate(
      base_tr = str_extract(base_tr, '(?<=(^| ))[^ ]*$'), # this captures both a / b and a formats of base. for a/b I need b because it was trained on that. it is hard coded which is bad.
      variant = transcribe(variant, 'single')
    )
  
  # let's see where the mgl found the relevant rule
  proc_dat = dat %>% 
    mutate(
      P2 = ifelse(is.na(P), '_', P),
      rule = glue('{A} -> {B} / {P2}'),
      'base_tr' = form1,
      'variant' = form2
    ) %>% 
    left_join(test) %>% 
    select(base_tr,variant,confidence,rule) %>% 
    arrange(base_tr)
  
  variants %>% 
    left_join(proc_dat) %>%
    filter(base_tr != 'fröjözik')
  
  # so the mgl weight for form1/form2 is confidence of rule that makes form1/confidence of rule that makes form2.
  # sometimes the mgl doesn't find a rule for form1 or form2. in those cases, I take the category weight for the other form as 1. in some cases it finds neither rules. in that case the weight is NA.
}

# take output of mglrules and cross it with test. the fun is split into two so you can see how rules are missing in the output before it gets crossed with test.
getMGLrules2 = function(dat,test){
  dat %>% 
    select(-rule) %>% 
    replace_na(list(confidence = 0)) %>% # if you don't find a rule its conf is naught
    group_by(base_tr,variant_number) %>% 
    arrange(-confidence) %>% 
    slice(1) %>% 
    ungroup() %>%
    pivot_wider(id_cols = base_tr, names_from = 'variant_number', values_from = confidence, values_fill = 0) %>% 
    mutate(adjusted_confidence = variant1/(variant1+variant2)) %>% 
    select(base_tr,adjusted_confidence) %>% 
    left_join(test)
}

# fit and tune the GCMs and save predictions from best model to disc. this horrible wrapper function exists because this takes a while.
fitGCMs = function(){

  # names must be fixed. gcm expects word.
  lakok_training %<>% rename("word" = base_tr)
  lakok_test %<>% rename("word" = base_tr)
  cselekszenek_training_1 %<>% rename("word" = base_tr)
  cselekszenek_training_2 %<>% rename("word" = base_tr)
  cselekszenek_training_3 %<>% rename("word" = base_tr)
  cselekszenek_test %<>% rename("word" = base_tr)
  hotelban_training_1 %<>% rename("word" = base_tr)
  hotelban_training_2 %<>% rename("word" = base_tr)
  hotelban_training_3 %<>% rename("word" = base_tr)
  hotelban_test %<>% rename("word" = base_tr)

  # for lakok/lakom, you map through values for s and fit the gcm on the training and test set
  lakok_gcms = map(seq(0,1,0.05), ~ tidyGCM(lakok_training, lakok_test, var_s = .))

  # for cselekszenek/cselekednek, 3 training sets. so you do this three times.
  cselekszenek_gcms_1 = map(seq(0,1,0.05), ~ tidyGCM(cselekszenek_training_1, cselekszenek_test, var_s = .))
  cselekszenek_gcms_2 = map(seq(0,1,0.05), ~ tidyGCM(cselekszenek_training_2, cselekszenek_test, var_s = .))
  cselekszenek_gcms_3 = map(seq(0,1,0.05), ~ tidyGCM(cselekszenek_training_3, cselekszenek_test, var_s = .))

  # for hotelban/hotelben, three training sets. so you fit this three times.
  hotelban_gcms_1 = map(seq(0,1,0.05), ~ tidyGCM(hotelban_training_1, hotelban_test, var_s = .))
  hotelban_gcms_2 = map(seq(0,1,0.05), ~ tidyGCM(hotelban_training_2, hotelban_test, var_s = .))
  hotelban_gcms_3 = map(seq(0,1,0.05), ~ tidyGCM(hotelban_training_3, hotelban_test, var_s = .))

  # you get the best GCM for all these.
  best_lakok = getBestGCM(lakok_gcms,lakok_test,lakok)
  best_cselekszenek_1 = getBestGCM(cselekszenek_gcms_1,cselekszenek_test,cselekszenek) %>% mutate(training_set = 'variable only')
  best_cselekszenek_2 = getBestGCM(cselekszenek_gcms_2,cselekszenek_test,cselekszenek) %>% mutate(training_set = 'stable only')
  best_cselekszenek_3 = getBestGCM(cselekszenek_gcms_3,cselekszenek_test,cselekszenek) %>% mutate(training_set = 'variable and stable')
  best_hotelban_1 = getBestGCM(hotelban_gcms_1,hotelban_test,hotelban) %>% mutate(training_set = 'variable only')
  best_hotelban_2 = getBestGCM(hotelban_gcms_2,hotelban_test,hotelban) %>% mutate(training_set = 'stable only')
  best_hotelban_3 = getBestGCM(hotelban_gcms_3,hotelban_test,hotelban) %>% mutate(training_set = 'variable and stable')

  # for csel. and hot. there are three best gcms to choose from.
  best_cselekszenek = bind_rows(best_cselekszenek_1,best_cselekszenek_2,best_cselekszenek_3) %>% 
    filter(r2 == max(r2))
  cselekszenek_gcms = cselekszenek_gcms_1 # hard coding this will come back to haunt us no doubt

  best_hotelban = bind_rows(best_hotelban_1,best_hotelban_2,best_hotelban_3) %>% 
    filter(r2 == max(r2))
  hotelban_gcms = hotelban_gcms_3 # hard coding this will come back to haunt us no doubt

  # for lak., csel., hot., find gcm with highest r2 and save it.
  lakok_gcms[[best_lakok$n]] %>% 
    mutate(
      s = seq(0,1,0.05)[best_lakok$n],
      r2 = best_lakok$r2,
      training_set = 'variable only'
    ) %>% 
    write_tsv('~/Github/Racz2024/models/learner/baseline/pred_gcm/lakok_gcm_baseline_pred.tsv')

  cselekszenek_gcms[[best_cselekszenek$n]] %>% 
    mutate(
      s = seq(0,1,0.05)[best_cselekszenek$n],
      r2 = best_cselekszenek$r2,
      training_set = best_cselekszenek$training_set
    ) %>% 
    write_tsv('~/Github/Racz2024/models/learner/baseline/pred_gcm/cselekszenek_gcm_baseline_pred.tsv')

  hotelban_gcms[[best_hotelban$n]] %>% 
    mutate(
      s = seq(0,1,0.05)[best_hotelban$n],
      r2 = best_hotelban$r2,
      training_set = best_hotelban$training_set
    ) %>% 
    write_tsv('~/Github/Racz2024/models/learner/baseline/pred_gcm/hotelban_gcm_baseline_pred.tsv')  
}

# set up training files for MGL and save them to disc. this horrible wrapper function exists because (a) takes a while (b) I have to fit the mgl-s by hand. not tuning here just playing around with training sets.
fitMGL = function(){

  # --- Forms are either A or B --- #

  ## lakok

  # create in file, save to disc
  buildMglInBinary(train = lakok_training, cat_name_1 = 'third_person', cat_name_2 = 'first_person', test = lakok_test, cselekszenek = F) %>% 
    write_lines(file = '~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_lakok/lakok_binary.in')  

  ## cselekszenek

  # we need an in file for training 1 - 2 - 3 AND each postag, since the mgl looks for morphological rules. these are different exponents and we shouldn't force the mgl to look for all the rules at once. unfair! bad.
  cs_t_1_n = cselekszenek_training_1 %>% 
    nestCselekszenek()

  # now we build the in file and save to disc for each nested data
  map2(cs_t_1_n$xpostag,cs_t_1_n$data, 
       ~ buildMglInBinary(train = .y, cat_name_1 = 'base', cat_name_2 = 'conj', test = cselekszenek_test, cselekszenek = T) %>% 
         write_lines(glue('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_cselekszenek/cselekszenek_1_{.x}_binary.in'))
       )

  # same logic here on out: training 2
  cs_t_2_n = cselekszenek_training_2 %>% 
    nestCselekszenek()

  map2(cs_t_2_n$xpostag,cs_t_2_n$data, 
       ~ buildMglInBinary(train = .y, cat_name_1 = 'base', cat_name_2 = 'conj', test = cselekszenek_test, cselekszenek = T) %>% 
         write_lines(glue('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_cselekszenek/cselekszenek_2_{.x}_binary.in'))
  )

  # ... training 3
  cs_t_3_n = cselekszenek_training_3 %>% 
    nestCselekszenek()

  map2(cs_t_3_n$xpostag,cs_t_3_n$data, 
       ~ buildMglInBinary(train = .y, cat_name_1 = 'base', cat_name_2 = 'conj', test = cselekszenek_test, cselekszenek = T) %>% 
         write_lines(glue('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_cselekszenek/cselekszenek_3_{.x}_binary.in'))
  )

  ## hotelban

  # same logic for hotelban. training 1 ...
  h_t_1_n = hotelban_training_1 %>% 
    nestHotelban()

  map2(h_t_1_n$xpostag,h_t_1_n$data, 
       ~ buildMglInBinary(train = .y, cat_name_1 = 'base', cat_name_2 = 'conj', test = hotelban_test, cselekszenek = F) %>% 
         write_lines(glue('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_hotelban/hotelban_1_{.x}_binary.in'))
  )

  # ... tr 2...
  h_t_2_n = hotelban_training_2 %>% 
    nestHotelban()

  map2(h_t_2_n$xpostag,h_t_2_n$data, 
       ~ buildMglInBinary(train = .y, cat_name_1 = 'base', cat_name_2 = 'conj', test = hotelban_test, cselekszenek = F) %>% 
         write_lines(glue('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_hotelban/hotelban_2_{.x}_binary.in'))
  )

  # ... and tr 3.
  h_t_3_n = hotelban_training_3 %>% 
    nestHotelban()

  map2(h_t_3_n$xpostag,h_t_3_n$data, 
       ~ buildMglInBinary(train = .y, cat_name_1 = 'base', cat_name_2 = 'conj', test = hotelban_test, cselekszenek = F) %>% 
         write_lines(glue('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_hotelban/hotelban_3_{.x}_binary.in'))
  )

  ##########################################
  # I need to run the MGL by hand. Using MinGenLearner.jar
  ##########################################
}

# -- wrangling -- #

dik = bind_rows(lakok,cselekszenek)
ik = filter(v, str_detect(lemma, 'ik$'))
hotelban %<>% filter(base %in% h) # full of silly words 
hotelban_plus %<>% 
  filter(lemma %in% h) %>%  # full of silly words
  mutate(category = 
           case_when(
             vh == 'front' ~ 'hotelben', 
             vh == 'back' ~ 'hotelban'
             )
         )

cselekszenek_postags = cselekszenek %>% 
  pull(xpostag) %>% 
  unique()

hotelban_postags = hotelban %>% 
  pull(xpostag) %>% 
  unique()

cselekszenek_plus %<>% 
  filterPlus(cselekszenek_postags)

hotelban_plus %<>%
  # rename('category' = vh) %>% 
  filterPlus(hotelban_postags)

# I need to create illicit sequences for the mgl. no two vowels next to each other.
vowels = c('a','á','e','é','i','í','o','ó','ö','ő','u','ú','ü','ű')
illicit_sequences = c()

# Generate combinations using nested loops
for (i in vowels) {
  for (j in vowels) {
    illicit_sequences = c(illicit_sequences, paste0(i, j))
  }
}

# Some more illicit sequences. These might not be justified.
illicit_sequences = c(illicit_sequences, "oze", "oza", "öze", "öza", "eze", "eza", "ode", "oda", "öde", "öda", "ede", "eda")

# -- training files -- #

## lakok
# it's one pair per verb stem. easy. lakok/lakom. 
lakok_training = lakok %>% 
  mutate(
    category = ifelse(log_odds >= median(log_odds),'lakok','lakom'),
    form = case_when(
      category == 'lakok' ~ form_1,
      category == 'lakom' ~ form_2
    ),
    form_tr = transcribe(form, 'single')
         ) %>%
  select(base,base_tr,form,form_tr,category,lemma_freq_corrected)

## cselekszenek

# many pairs for one verb. I want to quantify the bias for the stem, based on the individual suffixed forms
fit1 = glmer(cbind(freq_1,freq_2) ~ 1 + (1|base_tr) + (1|xpostag), family = binomial, data = cselekszenek)

cselekszenek_base = ranef(fit1) %>% 
  as_tibble() %>% 
  select(grp,condval) %>% 
  rename('base_tr' = grp, 'intercept' = condval)

cselekszenek_training_1 = cselekszenek_base %>% 
  select(base_tr,intercept) %>% 
  left_join(cselekszenek) %>% 
  mutate(
    category = ifelse(intercept >= median(intercept),'cselekszenek','cselekednek'),
    form = case_when(
      category == 'cselekszenek' ~ form_1,
      category == 'cselekednek' ~ form_2
    ),
    form_tr = transcribe(form, 'single')
  ) %>%
  select(base,base_tr,form,form_tr,category,lemma_freq_corrected,xpostag)

cselekszenek_training_2 = cselekszenek_plus %>% 
  mutate(
    base = lemma,
    category = ifelse(category == 'cvc','cselekednek','cselekszenek'),
    base_tr = transcribe(lemma, 'single'),
    form_tr = transcribe(form, 'single'),
    lemma_freq_corrected = lemma_freq
  ) %>% 
  select(base,base_tr,form,form_tr,category,lemma_freq_corrected,xpostag)

cselekszenek_training_3 = 
  bind_rows(cselekszenek_training_1,cselekszenek_training_2)    

## hotelban

# many pairs for one noun. I want to quantify the bias for the stem, based on the individual suffixed forms
fit2 = glmer(cbind(freq_1,freq_2) ~ 1 + (1|base_tr) + (1|xpostag), family = binomial, data = hotelban)

hotelban_base = ranef(fit2) %>% 
  as_tibble() %>% 
  select(grp,condval) %>% 
  rename('base_tr' = grp, 'intercept' = condval)

hotelban_training_1 = hotelban_base %>% 
  select(base_tr,intercept) %>% 
  left_join(hotelban) %>% 
  mutate(
    category = ifelse(intercept >= median(intercept),'hotelban','hotelben'),
    form = case_when(
      category == 'hotelban' ~ form_1,
      category == 'hotelben' ~ form_2
    ),
    form_tr = transcribe(form, 'single')
  ) %>%
  select(base,base_tr,form,form_tr,category,lemma_freq_corrected,xpostag)

hotelban_training_2 = hotelban_plus %>% 
  filter(category != 'mixed') %>% 
  mutate(
    base = lemma,
    category = ifelse(category == 'back','hotelban','hotelben'),
    base_tr = transcribe(lemma, 'single'),
    form_tr = transcribe(form, 'single'),
    lemma_freq_corrected = lemma_freq
  ) %>% 
  select(base,base_tr,form,form_tr,category,lemma_freq_corrected,xpostag)

hotelban_training_3 = 
  bind_rows(hotelban_training_1,hotelban_training_2)

# -- test files -- #

## lakok

lakok_test = b %>% 
  filter(variation == 'lakok/lakom') %>% 
  mutate(category = ifelse(log_odds >= median(log_odds),'lakok','lakom')) %>%
  select(base_tr,category,log_odds)

## cselekszenek

cselekszenek_test = b %>% 
  filter(variation == 'cselekszenek/cselekednek') %>% 
  mutate(
    category = ifelse(log_odds >= median(log_odds),'cselekszenek','cselekednek'),
    base = str_extract(base, '(?<=\\/ )[^ ]+'), # I need a recognisable stem. I pick the cvc form.
    base_tr = str_extract(base_tr, '(?<=\\/ )[^ ]+')
  ) %>% 
  select(base_tr,category,log_odds)

## hotelban

hotelban_test = b %>% 
  filter(variation == 'hotelban/hotelben') %>%
  mutate(
    category = ifelse(log_odds >= median(log_odds),'hotelban','hotelben'),
  ) %>% 
  select(base_tr,category,log_odds)  

# -- fitting GCM -- #

# This takes a while.
# fitGCM()

lakok_gcm = read_tsv('~/Github/Racz2024/models/learner/baseline/pred_gcm/lakok_gcm_baseline_pred.tsv')
cselekszenek_gcm = read_tsv('~/Github/Racz2024/models/learner/baseline/pred_gcm/cselekszenek_gcm_baseline_pred.tsv')
hotelban_gcm = read_tsv('~/Github/Racz2024/models/learner/baseline/pred_gcm/hotelban_gcm_baseline_pred.tsv')

# -- fitting the MGL -- #

# This has to be done by hand.
# fitMGL()

# get output from 

# lakok
lakok_master = read_tsv('~/Github/Racz2024/models/learner/baseline/mgls/rawmgls/mgl_lakok/lakok_binary.sum') # parsing issues expected
lakok_mgl = lakok_master %>% 
  getMGLrules(lakok_test, 'lakok/lakom')
lakok_mgl_2 = getMGLrules2(lakok_mgl,lakok_test)

# cselekszenek
cselekszenek_master = getMGLmaster('mgl_cselekszenek')

cselekszenek_master %<>% 
  group_by(training_type) %>% 
  nest() %>% 
  mutate(
    rules = map(data, ~ getMGLrules(.,cselekszenek_test,'cselekszenek/cselekednek')),
    test_rules = map(rules, ~ getMGLrules2(.,cselekszenek_test))
    )

cselekszenek_mgl = cselekszenek_master %>% 
  select(training_type,rules) %>% 
  unnest(cols = rules)

cselekszenek_mgl_2 = cselekszenek_master %>% 
  select(training_type,test_rules) %>% 
  unnest(cols = test_rules)

# hotelban
hotelban_master = getMGLmaster('mgl_hotelban')

hotelban_master %<>% 
  group_by(training_type) %>% 
  nest() %>% 
  mutate(
    rules = map(data, ~ getMGLrules(.,hotelban_test,'hotelban/hotelben')),
    test_rules = map(rules, ~ getMGLrules2(.,hotelban_test))
  )

hotelban_mgl = hotelban_master %>% 
  select(training_type,rules) %>% 
  unnest(cols = rules)

hotelban_mgl_2 = hotelban_master %>% 
  select(training_type,test_rules) %>% 
  unnest(cols = test_rules)

# -- save mgl -- #

write_tsv(lakok_mgl, 'pred_mgl/lakok_mgl_rules.tsv')
write_tsv(cselekszenek_mgl, 'pred_mgl/cselekszenek_mgl_rules.tsv')
write_tsv(hotelban_mgl, 'pred_mgl/hotelban_mgl_rules.tsv')

write_tsv(lakok_mgl_2, 'pred_mgl/lakok_mgl_baseline.tsv')
write_tsv(cselekszenek_mgl_2, 'pred_mgl/cselekszenek_mgl_baseline.tsv')
write_tsv(hotelban_mgl_2, 'pred_mgl/hotelban_mgl_baseline.tsv')
