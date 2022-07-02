setwd('~/Github/Racz2024/')

library(tidyverse)
library(magrittr)
library(glue)
library(h2o)

set.seed(1337)

# -- setting up -- #

buildBigrams = function(s){
  s = glue('#{s}#')
  slength = nchar(s)
  
  if (slength == 2){
    print('Input string has zero length.')
    break
    }
  
  bigrams = as.list(NULL)
  for (i in 1:(slength-1)){
    sbar = str_sub(s, i, slength)
    bigrams[[i]] = str_sub(sbar, 1, 2)
  }
  bigrams = unlist(bigrams)
  return(bigrams)
}

formatData = function(dat){
  dat %>% 
  select(base,base_tr,log_odds) %>% # take base, transcribed base and log odds of outcome
    rowwise() %>% # for each row
    mutate(bigram = list(buildBigrams(base_tr))) %>% # split the transcribed base into bigrams
    select(base,log_odds,bigram) %>% 
    unnest(cols = c(bigram)) %>% # build a long df where one row is one bigram per base 
    count(base,log_odds,bigram) %>% #count bigrams per base (in case there's more than one in a base)
    pivot_wider(id_cols = c(base,log_odds), names_from = bigram, values_from = n, values_fill = 0) %>%  # tally up how many times EVERY bigram is in one word (it will mostly be 0)
    sample_n(nrow(dat)) # we shuffle things for the neural network
}

d = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
e = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')

d %<>% formatData()
e %<>% formatData()

# -- modelling -- #
# with an eye on https://htmlpreview.github.io/?https://github.com/ledell/sldm4-h2o/blob/master/sldm4-deeplearning-h2o.html

h2o.init(ip="localhost", port=54321, max_mem_size="16g", nthreads=8)

train = as.h2o(d[1:654,])
valid = as.h2o(d[655:976,])
test = as.h2o(e) # not the same cols as in train/valid, we'll do something about that at some point.

x = names(d)[!names(d) %in% c('base','log_odds')]
y = 'log_odds'

fit1 = h2o.deeplearning(
  x = x,
  y = y,
  training_frame = train,
  validation_frame = valid,
  model_id = "fit1",
  hidden = c(20,20),
  seed = 1,
  distribution = 'Gaussian',
  stopping_metric = 'deviance'
  )

fit2 = h2o.deeplearning(
  x = x,
  y = y,
  training_frame = train,
  validation_frame = valid,
  model_id = "fit2",
  epochs = 50,
  stopping_rounds = 0,
  hidden = c(20,20),
  seed = 1,
  nfolds = 4,
  distribution = 'Gaussian'
)

fit3 = h2o.deeplearning(
  x = x,
  y = y,
  training_frame = train,
  validation_frame = valid, # I'm not sure how this squares with cv
  model_id = "fit2",
  epochs = 50,
  hidden = c(20,20),
  seed = 1,
  nfolds = 4,
  distribution = 'Gaussian',
  stopping_metric = 'deviance',
  score_interval = 1,
  stopping_rounds = 5,
  stopping_tolerance = 1e-3
)

h2o.mse(fit1);h2o.mse(fit2);h2o.mse(fit3)
plot(fit1, timestep = "epochs", metric = "rmse")
plot(fit2, timestep = "epochs", metric = "rmse")
plot(fit3, timestep = "epochs", metric = "rmse")
# this does not look good in many ways


valid$pred1 = h2o.predict(fit1, newdata = train)
train$pred2 = h2o.predict(fit2, newdata = train)

d1 = as_tibble(train)
d1 %>% 
  select(base,log_odds,pred1,pred2) %>% 
  pivot_longer(-c(base,log_odds)) %>% 
  ggplot(aes(log_odds,value)) +
  geom_point() +
  facet_wrap(~ name) +
  theme_bw()

# -- grid search -- #

hyper = list(
  l1 = c(0,0.1),
  epochs = c(10,30),
  hidden = c(c(30,30),c(100),c(20,20,20)),
  input_dropout_ratio = c(0.1,0.2),
  hidden_dropout_ratios = c(0.1,0.4)
)

grid1 = h2o.grid(
  'deeplearning',
  x = x,
  y = y,
  training_frame = train,
  validation_frame = valid,
  grid_id = "grid1",
  activation = 'RectifierWithDropout',
  seed = 1,
  distribution = 'Gaussian',
  stopping_metric = 'deviance',
  hyper_params = hyper
)

gridz = h2o.getGrid(grid_id = "grid1", 
            sort_by = "mae", 
            decreasing = FALSE)

best = gridz@model_ids[[1]]
best = h2o.getModel(best)

h2o.performance(best)
plot(best, timestep = "epochs", metric = "rmse")

train$pred = h2o.predict(best, newdata = train)
valid$pred = h2o.predict(best, newdata = valid)
test$pred = h2o.predict(best, newdata = test)

as_tibble(train) %>% 
  ggplot(aes(log_odds,pred)) +
  geom_point() +
  theme_bw()

as_tibble(valid) %>% 
  ggplot(aes(log_odds,pred)) +
  geom_point() +
  theme_bw()

as_tibble(test) %>% 
  ggplot(aes(log_odds,pred)) +
  geom_point() +
  theme_bw()

as_tibble(train) %>% 
  summarise(r2 = cor(log_odds,pred))
as_tibble(valid) %>% 
  summarise(r2 = cor(log_odds,pred))
as_tibble(test) %>% 
  summarise(r2 = cor(log_odds,pred)) # kinos
