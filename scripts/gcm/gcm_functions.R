# take data with log odds, add quantiles
addQ = function(dat){
dat %>%
mutate(
    n14 = quantile(log_odds, probs = .25),
    n34 = quantile(log_odds, probs = .75),
    n13 = quantile(log_odds, probs = .33),
    n23 = quantile(log_odds, probs = .66)
  )
}  

# take training data and test data, build a big nested tibble of various category limits based on qq-s as well as values for the s parameter, map the gcm through it, return nested df of predictions
iterateBaselineGCM = function(training_data,test_data){
  tibble(
    quantiles = c('fifty','thirty','twenty-five'),
    training_col = c(
      list(
        training_data %>%
          mutate(category = case_when(
            log_odds >= 0 ~ 'cat_1',
            log_odds < 0 ~ 'cat_2'
          )
          ) %>% 
          filter(!is.na(category)) %>%
          select(word,category)
      ),
      list(
        training_data %>%
          mutate(category = case_when(
            log_odds > n23 ~ 'cat_1',
            log_odds < n13 ~ 'cat_2'
          )
          ) %>% 
          filter(!is.na(category)) %>%
          select(word,category)
      ),
      list(
        training_data %>%
          mutate(category = case_when(
            log_odds > n34 ~ 'cat_1',
            log_odds < n14 ~ 'cat_2'
          )
          ) %>% 
          filter(!is.na(category)) %>%
          select(word,category)
      )
    )
  ) %>% 
    crossing(
      var_s = seq(0.1,0.9,0.1)
    ) %>% 
    mutate(
      model = map2(training_col, var_s, ~ tidyGCM(training = .x, test = test_data, var_s = .y, var_p = 1, distance_metric = 'lv')
      )
    )
}

# take output of iterateGCM pick the best model based on r, return predictions of best model and attributes of best model
mineGCMtibble = function(dat_in,dat_b){
  dat_in %>% 
  select(quantiles,var_s,model) %>% 
  mutate(
    pred = map(model, ~ left_join(., dat_b, by = 'word')),
    r = map(pred, ~ summarise(., r = cor(cat_1,log_odds))) %>% 
      unlist()
  ) %>% 
  filter(r == max(r)) %>% 
  mutate(baseline_gcm_info = glue('q: {quantiles}, s: {var_s}, r: {round(r,2)}')) %>% 
  select(-model) %>% 
  unnest(cols = c(pred)) %>% 
  select(word,cat_1,log_odds,baseline_gcm_info)
}

# take esp master and grab relevant variation across the lists
grabESPtraining = function(my_variation){
  esp %>% 
    filter(variation == my_variation) %>% 
    mutate(
      category = ifelse(esp_response == variant1, 'cat_1', 'cat_2'),
      word = transcribe(base, 'single')
    ) %>% 
    select(word,category,file_name,list_number) %>% 
    nest(-c(file_name,list_number))
}

# take the best gcm training data and the esp data, figures out the posttest lists for each esp set, combines the baseline training and the esp input for each esp set, fits gcm, grabs predictions. s needs to be looked up and fixed separately, it just happens to be .9 for lakok and cselekszenek. 
iterateESPGCM = function(esp_dat,training_dat,my_s){  
  # to quote the hesp script:
  # rollPostTest = function(stimuli_num){
  #   if (stimuli_num < 4) {
  #     stimuli_post_test_num = 4;
  #   } else if (stimuli_num > 3 & stimuli_num < 8) {
  #     stimuli_post_test_num = 8;
  #   } else {
  #     stimuli_post_test_num = 1;
  #   }
  # return(stimuli_post_test_num)  
  # }
l4 = esp_dat %>% 
  filter(list_number == 4) %>% 
  unnest(cols = data) %>% 
  select(word)
l8 = esp_dat %>% 
  filter(list_number == 8) %>% 
  unnest(cols = data) %>% 
  select(word)
l1 = esp_dat %>% 
  filter(list_number == 1) %>% 
  unnest(cols = data) %>% 
  select(word)

esp_dat %<>% 
  mutate(
    etest = case_when(
      list_number %in% 0:3 ~ list(l4),
      list_number %in% 4:7 ~ list(l8),
      list_number %in% 8:11 ~ list(l1)
      ),
    etraining = map(data, ~ bind_rows(.,training_dat))
    )

esp_dat %>% 
  mutate(
    model = map2(etraining, etest, ~ tidyGCM(training = .x, test = .y, var_s = my_s, var_p = 1, distance_metric = 'lv')) 
  ) %>% 
  unnest(cols = model) %>%
  select(file_name,list_number,word,cat_1)
}