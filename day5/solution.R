library(tidyverse)
library(lubridate)
library(tidytext)

#day 5 Part 1

polymer <- read_csv("day5/input.txt", col_names = FALSE)
polymers <- polymer$X1

length1 <- 1
length2 <- 2

while (length1 != length2) {
  length1 <- nchar(polymers)
  polymers <- gsub("aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ|Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz", replacement = "", 
                   x = polymers)
  length2 <- nchar(polymers)
}

nchar(polymers) #answer

#Part 2

unit1 <- paste0(letters, LETTERS)
unit2 <- paste0(LETTERS, letters)

lengths <- c()

for (i in 1:length(letters)) {
   polymers <- polymer$X1
   polymers <- gsub(paste0("[", letters[i], "|", LETTERS[i], "]"), "", polymers)
   length1 <- 1 
   length2 <- 2
   while(length1 != length2) {
     length1 <- nchar(polymers)
     polymers <- gsub("aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ|Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz", replacement = "", 
                      x = polymers)
     length2 <- nchar(polymers)
   }
   lengths[i] <- nchar(polymers)
}

min(lengths) #answer

      