# getting other movie information from IMBD

library(readr)
library(tidyverse)

dat <- read_tsv("/Users/dujamichael/Downloads/title.basics.tsv.gz") %>% 
  filter(startYear>2017)
         
dat1 <- dat %>% filter(titleType=="movie"|titleType=="tvMovie")


reviews <- read_tsv("/Users/dujamichael/Downloads/title.ratings.tsv.gz")
joined <- left_join(dat1,reviews, by="tconst") %>% 
  rename(movie_name = primaryTitle,
         release_year=startYear)

withratings <- left_join(merged_unique, joined)
