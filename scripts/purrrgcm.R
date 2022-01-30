##########################################################################################
##########################################################################################
# Generalised Context Model
# v0.1
# author: Péter Rácz 
# github: @petyaracz
# orig: Nosofsky 1988
##########################################################################################
##########################################################################################

require(tidyverse)

# we expect a test set as a tibble with one column called word as character vector
# we expect a training set as a tibble with two columns called word as character vector and category as character vector
# category identifies distinct categories
# one word is member of one category
# each word is member of one category
# var s, var p: look up the Generalised Context Model.
# s defaults to 0.3
# p defaults to 1
# distance_metric: defaults to lv. only tested with lv, who knows what happens if you use sg else!!

tidyGCM = function(training,test,var_s=0.3,var_p=1,distance_metric='lv'){

training = training %>% droplevels 
test = test %>% droplevels
  
getTargetSimilarity = function(target,training){

# we drop target from training in case we are cross-validating  
training = training %>% 
  filter(word != target)

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

# for each target in test, get ftdc.wide using getTargetsimilarity and put into new cell. unnest. we won.  
result = test %>% 
  mutate(similarities = purrr::map(word, function(x) getTargetSimilarity(x, training))) %>% 
  select(word, similarities) %>% 
  unnest

return(result)
}