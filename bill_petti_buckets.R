

library(tidyverse)

## Thanks to Bill Petti @ Slack
set.seed(42)
df <- tibble(amount = runif(100, 
                            min = 1,
                            max = 50), 
             group = rep(x = 1:10,
                         each = 10))
df %>%
  group_by(group) %>%
  summarise(average = mean(amount))

## Kevin K. 

df %>%
  mutate(group = as.factor((amount %/% 10) + 1)) %>%
  group_by(group) %>%
  summarise(average = mean(amount)) 

## Camille 

zoo::rollmean()  (and more generally zoo::rollapply() ) :
  
zoo::rollmean(x = 1:20, k = 10)
 #  5.5  6.5  7.5  8.5  9.5 10.5 11.5 12.5 13.5 14.5
 # 15.5