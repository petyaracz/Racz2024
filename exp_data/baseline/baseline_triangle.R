library(tidyverse)
library(glue)
library(ggthemes)
library(caret)
library(broom)
library(patchwork)

bl = read_tsv('~/Github/Racz2024/exp_data/baseline/baseline_tidy.tsv')

makeTriangle = function(dat){
  
  dat2 = dat %>% 
    select(file_name,word,resp_is_first_variant)
  
  dat_w = dat2 %>% 
    count(word,resp_is_first_variant) %>% 
    pivot_wider(word, names_from = resp_is_first_variant, values_from = n) %>% 
    mutate(p_word = `TRUE`/(`TRUE`+`FALSE`)) %>% 
    select(word,p_word)
  
  dat_p = dat2 %>% 
    count(file_name,resp_is_first_variant) %>% 
    pivot_wider(file_name, names_from = resp_is_first_variant, values_from = n, values_fill = 0) %>%
    mutate(p_id = `TRUE`/(`TRUE`+`FALSE`)) %>% 
    select(file_name,p_id)
  
  dat2 %>% 
    inner_join(dat_w, by = 'word') %>% 
    inner_join(dat_p, by = 'file_name') %>% 
    filter(!is.na(p_word),!is.na(p_id)) %>% 
    mutate(
      word = fct_reorder(word, p_word),
      file_name = fct_reorder(file_name, p_id),
      ideal_model = p_word * p_id,
      ideal_model_sig = qlogis(p_word) * qlogis(p_id),
      resp_ideal_model = ideal_model > mean(ideal_model)
    )
}

drawTriangle = function(dat,name,var){
  dat %>% 
    ggplot(aes(file_name,word,fill = {{var}})) +
    geom_tile() +
    theme_few() +
    scale_fill_colorblind() +
    ggtitle(name) +
    guides(fill = 'none') +
    theme(axis.text = element_blank()) +
    xlab('participant (low -> high)') +
    ylab('word (low -> high)')
}  

getTriangleMatrix = function(dat){
  caret::confusionMatrix(
    as.factor(dat$resp_ideal_model),
    as.factor(dat$resp_is_first_variant)
  ) %>% 
    broom::tidy() %>% 
    slice(1)
}

triangles = bl %>% 
  group_by(my_list,category) %>% 
  nest() %>% 
  mutate(
    set_name = glue('{category} {str_extract(my_list, "[123]")}'),
    set_triangle = map(data, ~ makeTriangle(.)),
    graph_triangle = map2(set_triangle, set_name, ~ drawTriangle(.x,.y,resp_is_first_variant)),
    graph_ideal = map2(set_triangle, set_name, ~ drawTriangle(.x,.y,resp_ideal_model)),
    graph_ideal2 = map2(set_triangle, set_name, ~ drawTriangle(.x,.y,resp_ideal_model)),
    test = map(set_triangle, ~ getTriangleMatrix(.))
  )

triangles_plots = triangles %>% 
  arrange(set_name) %>% 
  pull(graph_triangle)
   
wrap_plots(triangles_plots, ncol = 3)

ideal_plots = triangles %>% 
  arrange(set_name) %>% 
  pull(graph_ideal)

wrap_plots(ideal_plots, ncol = 3)

ideal_plots2 = triangles %>% 
  arrange(set_name) %>% 
  pull(graph_ideal2)

wrap_plots(ideal_plots2, ncol = 3)

triangles %>% 
  select(set_name,test) %>% 
  unnest(test) %>% 
  ungroup() %>% 
  select(set_name,estimate,conf.low,conf.high) %>% 
  write_tsv('baseline_triangle_acc.tsv')
