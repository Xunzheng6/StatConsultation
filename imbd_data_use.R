# getting other movie information from IMDB

library(readr)
library(tidyverse)
library(splitstackshape)
library(stringi)
library(tm)

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

joined$movie_name <-str_replace_all(joined$movie_name,':','')
joined$movie_name <-str_replace_all(joined$movie_name,'&','and')



withratings <- left_join(unique, joined, by="movie_name")
withratings <- withratings %>% filter(release_year <= year) %>% 
  group_by(movie_name) %>% mutate(n_matches=n(),
                                  unclear=ifelse(n_matches>1,1,0))
table(withratings$n_matches) # a good percentage is matched.

# get the genres:
gens <- unique(word(withratings$genres, sep=","))

# make them into indicator columns:
withratings$Documentary <- grepl("Documentary", withratings$genres)
withratings$Drama <- grepl("Drama", withratings$genres)
withratings$Animation <- grepl("Animation", withratings$genres)
withratings$Comedy <- grepl("Comedy", withratings$genres)
withratings$Crime <- grepl("Crime", withratings$genres)
withratings$Action <- grepl("Action", withratings$genres)
withratings$Adventure <- grepl("Adventure", withratings$genres)
withratings$Biography <- grepl("Biography", withratings$genres)
withratings$Horror <- grepl("Horror", withratings$genres)
withratings$Mystery <- grepl("Mystery", withratings$genres)
withratings$Thriller <- grepl("Thriller", withratings$genres)
withratings$SciFi <- grepl("Sci-Fi", withratings$genres)
withratings$Fantasy <- grepl("Fantasy", withratings$genres)
withratings$Romance <- grepl("Romance", withratings$genres)
withratings$Family <- grepl("Family", withratings$genres)
withratings$Musical <- grepl("Musical", withratings$genres)
withratings$Music <- grepl("Music", withratings$genres)
withratings$Adult <- grepl("Adult", withratings$genres)

fuzzy <- withratings %>% filter(n_matches>1) %>% select(author,movie_name, 
                                                        originalTitle,n_matches, averageRating,numVotes,
                                                        year,release_year)

table(withratings$n_matches). 
# 1651 out of 2039 were uniquely matched to a review, roughly 80%. Not bad.

# we will drop those who were matched fuzzily in the respective analyses.

