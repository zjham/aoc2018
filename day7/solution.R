library(tidyverse)
library(ggraph)
library(igraph)

input <- read_csv("day7/input.txt", col_names = FALSE) 
theme_set(theme_void())

steps <- input %>%
  mutate(step1 = substr(X1, 6, 6),
         step2 = substr(X1, 37, 37)) %>%
  select(-X1)

graph <- graph_from_data_frame(steps)

ggraph(graph) +
  geom_edge_link(arrow = grid::arrow())

#Need to define a way to check if steps are ready.If they are in the second column they are not ready. 
order <- c()
i <- 1
while (nrow(steps) >= 1) {
  ready <- LETTERS[which(!(LETTERS %in% steps$step2) & 
                         !(LETTERS %in% unlist(order)))][1]
  order <- append(order, ready)
  steps <- filter(steps, !(step1 %in% ready))
}

paste0(paste(unlist(order), collapse = ""), "E") #E does not appear in first column, but still needs to be executed last


#Day 7 Part 2








