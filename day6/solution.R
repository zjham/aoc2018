library(tidyverse)
library(lubridate)
library(tidytext)

#Day 6 Part 1
input <- read_csv("day6/input.txt", col_names = FALSE)

input <- input %>%
  mutate(X = X1, Y = X2) %>%
  select(-X1, -X2) 

input %>%
  summarize(min(X), min(Y), max(X), max(Y))



nums <- crossing(crossing(x = 48:347, y = 42:338), input) %>%
    mutate(dist = abs(X-x) + abs(Y-y)) %>% 
    group_by(x, y) %>%
    filter(dist == min(dist)) %>%
    mutate(n = n()) %>%
    filter(n == 1) %>%
    select(-n) %>%
    ungroup() %>%
    count(X, Y, sort = TRUE) #answer
  
input %>%
  ggplot(aes(X, Y)) +
  geom_point(size = 4) #use graph to visually rule out points with infinite areas.

#Day6 Part 2


nums <- crossing(crossing(x = 0:500, y = 0:500), input) %>%
  mutate(dist = abs(X-x) + abs(Y-y))

nums %>%
  group_by(x, y) %>%
  summarize(total_dist = sum(dist)) %>%
  ungroup() %>%
  filter(total_dist < 10000) #length of tibble is answer


