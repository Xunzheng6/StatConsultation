# getting other movie information from IMBD

library(readr)
library(tidyverse)

dat <- read_tsv("/Users/dujamichael/Downloads/title.basics.tsv.gz") %>% 
  filter(startYear>2016)
         
dat1 <- dat %>% filter(titleType=="movie"|titleType=="tvMovie")


reviews <- read_tsv("/Users/dujamichael/Downloads/title.ratings.tsv.gz")
joined <- left_join(dat1,reviews, by="tconst") %>% 
  rename(movie_name = primaryTitle,
         release_year=startYear) %>% 
  mutate(release_year=as.numeric(release_year))

# Clean up movie names ; replace dashes with spaces ; remove white space ; lower case all for movie names
joined$movie_name <-str_replace(joined$movie_name, " \\s*\\([^\\)]+\\)", "")
joined$movie_name <-str_replace_all(joined$movie_name,'-',' ')
joined$movie_name <- trimws(joined$movie_name)
joined$movie_name <- tolower(joined$movie_name)


withratings <- left_join(unique, joined, by="movie_name")
withratings <- withratings %>% filter(release_year <= year) %>% 
  group_by(movie_name) %>% mutate(n_matches=n(),
                                  unclear=ifelse(n_matches>1,1,0))
chex <- withratings %>% group_by(movie_name) %>% summarise(unclear) 
withratings <- withratings %>% distinct(movie_name, .keep_all = TRUE)
sum(is.na(withratings$tconst))

