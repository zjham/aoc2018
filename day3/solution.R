library(tidyverse)

ids <- tbl_df(readLines("day3/input.txt"))

#Day 3 Part 1
ids <- ids %>%
  separate(value, c("id", "at", "pos", "dim"), sep = " ") %>%
  select(-at) %>%
  mutate(id = parse_number(id)) %>%
  separate(col = pos, into = c("pos1", "pos2"), sep = ",") %>%
  mutate(pos2 = parse_number(pos2)) %>%
  separate(dim, into = c("dim1", "dim2"), sep = "[a-z]") %>%
  mutate_if(is.character, as.numeric)

m <- matrix(0, nrow = 1000, ncol = 1000)

for (i in 1:nrow(ids)) {
  indx <- seq(ids$pos1[i] + 1, ids$pos1[i] + ids$dim1[i])
  indy <- seq(ids$pos2[i] + 1, ids$pos2[i] + ids$dim2[i])
  
  for (k in indx) {
    for (j in indy) {
    
      m[k, j] <- m[k, j] + 1
      
    }
  }
}
length(m[m>1])


#Day3 Part 2
m_tidy <- tbl_df(m) %>%
  mutate(row = row_number())

m_tidy <- m_tidy %>%
  gather(key = "col", value = "value", -row) %>%
  mutate(col = parse_number(col),
         ind = paste(row, col)) %>%
  filter(value == 1)

suit <- c()
for (i in 1:nrow(ids)) {
  indx <- seq(ids$pos1[i] + 1, ids$pos1[i] + ids$dim1[i])
  indy <- seq(ids$pos2[i] + 1, ids$pos2[i] + ids$dim2[i])
  index <-  crossing(indx, indy) %>% mutate(ind = paste(indx, indy))
  suit[i] <- all(index$ind %in% m_tidy$ind)
}

which(suit)


