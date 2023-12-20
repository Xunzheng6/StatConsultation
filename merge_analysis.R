setwd("~/Desktop/A3SR/STATS CONSULTING/StatConsultation/")

library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(readr)
library(tidyverse)
library(splitstackshape)
library(stringi)
library(tm)
library(ggplot2)


################
# getting other movie information from IMDB
###############

imdbTSVfiles <- function(fileName){
  url <- paste0("https://datasets.imdbws.com/",fileName,".tsv.gz")
  tmp <- tempfile()
  download.file(url, tmp)
  
  assign(fileName,
         readr::read_tsv(
           file = gzfile(tmp),
           col_names = TRUE,
           quote = "",
           na = "\\N"),
         envir = .GlobalEnv)
}

imdbTSVfiles("title.basics")
imdbTSVfiles("title.ratings")


titles <- title.basics %>% 
  filter(startYear>2016,
         titleType=="movie"|titleType=="tvMovie")


ratings <- title.ratings
joined <- left_join(titles,ratings, by="tconst") %>% 
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

################
# loading API movie data
###############

# appending the data
Movie2018H1 <- load("Movie2018H1.Rdata")
Movie2018H1 <- MovieFile
Movie2018H2 <- load("Movie2018H2.Rdata")
Movie2018H2 <- MovieFile
Movie2019H1 <- load("Movie2019H1.Rdata")
Movie2019H1 <- MovieFile
Movie2019H2 <- load("Movie2019H2.Rdata")
Movie2019H2 <- MovieFile
Movie2020H1 <- load("~/Desktop/A3SR/STATS CONSULTING/StatConsultation/Movie2020H1.Rdata")
Movie2020H1 <- MovieFile
Movie2020H2 <- load("~/Desktop/A3SR/STATS CONSULTING/StatConsultation/Movie2020H2.Rdata")
Movie2020H2 <- MovieFile
load("Movie2021H1.Rdata")
load("Movie2021H2.Rdata")
load("Movie2022H1.Rdata")
load("Movie2022H2.Rdata")
load("Movie2023H1.Rdata")
load("Movie2023H2.Rdata")


merged_all <- rbind(Movie2018H1, Movie2018H2, 
                    Movie2019H1, Movie2019H2,
                    Movie2020H1, Movie2020H2,
                    Movie2021H1, Movie2021H2, 
                    Movie2022H1, Movie2022H2, 
                    Movie2023H1, Movie2023H2)

# keeping only movies using the /movies/ pattern in response.docs.web_url
merged <- merged_all %>%
  filter(str_detect(response.docs.web_url, "/movies"))
# we have 4268 movies, now keep only unique ones

#merged_unique <- unique(merged)
merged_unique <- merged %>% distinct(response.docs.snippet, .keep_all = TRUE)
#only 4142 unique!

# Add author name (removing "By")
stopwords = c("By")
merged_unique$author <- gsub("By","",as.character(merged_unique$response.docs.byline.original))

# clean critic's pick column
merged_unique$criticpick <- ifelse(is.na(merged_unique$response.docs.headline.kicker),0,1)
table(merged_unique$criticpick)

# clean date column
merged_unique <- merged_unique %>%
  mutate (month = month(response.docs.pub_date), 
          year = year(response.docs.pub_date))
merged_unique$monthyear <- as.yearmon(paste(merged_unique$year, merged_unique$month), "%Y %m")


merged_unique <- merged_unique %>% mutate(type=0,
                                          movie_name=0)
for (i in 1:nrow(merged_unique)) {
  typetable=as.data.frame(merged_unique$response.docs.keywords[i])
  merged_unique$type[i]=ifelse(any(typetable$value=="Documentary Films and Programs"),"Documentary Films and Programs",
                               ifelse(any(typetable$value=="Movies"),"Movies",
                                      ifelse(any(typetable$value=="Animated Films"),"Animated Films",NA)))
  merged_unique$movie_name1[i] <- typetable[typetable$name=="creative_works", "value"][1]
}
check2 <- merged_unique %>% filter (is.na(merged_unique$type))

merged_unique$movie_name2 <- str_match(merged_unique$response.docs.web_url, "/movies/\\s*(.*?)\\s*-review")[,2]
merged_unique$movie_name <- ifelse(is.na(merged_unique$movie_name1),merged_unique$movie_name2,merged_unique$movie_name1)
check3 <- merged_unique %>% filter(is.na(movie_name)) #only 3; can live without

# remove (Movie) ; replace dashes with spaces ; remove white space ; lower case all for movie names
merged_unique$movie_name <-str_replace(merged_unique$movie_name, " \\s*\\([^\\)]+\\)", "")
merged_unique$movie_name <-str_replace_all(merged_unique$movie_name,'-',' ')
merged_unique$movie_name <- trimws(merged_unique$movie_name)
merged_unique$movie_name <- tolower(merged_unique$movie_name)
# replace & with "and", remove ":"
merged_unique$movie_name <-str_replace_all(merged_unique$movie_name,':','')
merged_unique$movie_name <-str_replace_all(merged_unique$movie_name,'&','and')


# trend number of movie reviews
api_unique <- merged_unique %>% dplyr::select (response.docs.pub_date, month, year, monthyear, 
                                   response.docs.news_desk, type, author, criticpick,
                                   movie_name, movie_name1, movie_name2)
api_unique <- api_unique %>% group_by(monthyear) %>% mutate(n_month=n()) %>% ungroup() %>% group_by(year) %>% 
  mutate(n_year=n()) %>% ungroup() %>% group_by(monthyear) %>% arrange(monthyear)


################
# Merging API and IMDb on movie name:
###############

withratings <- left_join(api_unique, joined, by="movie_name") #5350
# keep only the joins where release date is BEFORE review date
withratings <- withratings %>% filter(release_year <= year) %>% 
  group_by(movie_name) %>% mutate(n_matches=n(),
                                  unclear=ifelse(n_matches>1,1,0)) #4456 row left
table(withratings$n_matches, useNA = "always") # 3232 uniquely matched out of 4142
3232/4142

#keep only matched movies
matched <- withratings %>% filter(n_matches==1)

################
# Extracting useful IMDb info
###############

# get the genres:
gens <- unique(word(matched$genres, sep=","))
length(gens) #19 genres

# make them into indicator columns:
matched$Documentary <- grepl("Documentary", matched$genres)
matched$Drama <- grepl("Drama", matched$genres)
matched$Animation <- grepl("Animation", matched$genres)
matched$Comedy <- grepl("Comedy", matched$genres)
matched$Crime <- grepl("Crime", matched$genres)
matched$Action <- grepl("Action", matched$genres)
matched$Adventure <- grepl("Adventure", matched$genres)
matched$Biography <- grepl("Biography", matched$genres)
matched$Horror <- grepl("Horror", matched$genres)
matched$Mystery <- grepl("Mystery", matched$genres)
matched$Animation <- grepl("Animation", matched$genres)
matched$Thriller <- grepl("Thriller", matched$genres)
matched$SciFi <- grepl("Sci-Fi", matched$genres)
matched$Fantasy <- grepl("Fantasy", matched$genres)
matched$Family <- grepl("Family", matched$genres)
matched$Musical <- grepl("Musical", matched$genres)
matched$History <- grepl("History", matched$genres)
matched$Music <- grepl("Music", matched$genres)
matched$Romance <- grepl("Romance", matched$genres)


table(matched$Documentary) #874
table(matched$Drama) # 1635
table(matched$Comedy) #759 
table(matched$Crime) #349
table(matched$Action) #377 
table(matched$Adventure) #324 
table(matched$Biography) #332
table(matched$Horror) #290
table(matched$Mystery) #243
table(matched$Animation) #130
table(matched$Thriller) #378
table(matched$SciFi) #114 
table(matched$Fantasy) #138
table(matched$Family) #88
table(matched$Musical) #30
table(matched$History) #182
table(matched$Music) #192
table(matched$Romance) #297

################
# PLOTS
###############


#review trends
api_unique %>%
  arrange(monthyear) %>% 
  group_by(monthyear, year) %>% 
  summarise(n_month=n()) %>% 
  ggplot() +
  aes(x = monthyear, y = n_month) +
  geom_line()+
  scale_color_gradient() +
  theme_minimal() 

#distribution of reviewed movies across genres
gens <- gens[-14]
gencounts <- c(sum(matched$Documentary==1), sum(matched$Drama==1),
               sum(matched$Comedy==1), sum(matched$Crime==1),
               sum(matched$Action==1), sum(matched$Adventure==1),
               sum(matched$Biography==1), sum(matched$Horror==1),
               sum(matched$Mystery==1), sum(matched$Animation==1),
               sum(matched$Thriller==1), sum(matched$SciFi==1),
               sum(matched$Fantasy==1), sum(matched$Family==1),
               sum(matched$Musical==1), sum(matched$History==1),
               sum(matched$Music==1), sum(matched$Romance==1))
genre_table=as.data.frame(cbind(gens,gencounts))
genre_table$gencounts <- as.numeric(genre_table$gencounts)
genre_table <- genre_table[order(genre_table$gencounts, decreasing = TRUE), ]

p <- ggplot(genre_table, aes(x = reorder(gens, +gencounts), y = gencounts)) + 
  geom_bar(stat="identity", color='red',fill="#112446") +
  coord_flip() 
p

#number of reviews per author
auth_rate <- matched %>% group_by(author) %>% 
  summarise(mean_imdb_rating=mean(averageRating, na.rm=TRUE),
            n_reviews=n())
auth_rate <- na.omit(auth_rate)

p <- ggplot(auth_rate, aes(x = reorder(author, +n_reviews), y = n_reviews)) + 
  geom_bar(stat="identity", color='red',fill="#112446") +
  coord_flip() 
p


#average imdb rating per author (those with more than 10 reviews)
auth_rate <- matched %>% group_by(author) %>% 
  summarise(mean_imdb_rating=mean(averageRating, na.rm=TRUE),
            n_reviews=n()) %>% 
  filter(n_reviews>10)
auth_rate <- na.omit(auth_rate)
p <- ggplot(auth_rate, aes(x = reorder(author, +mean_imdb_rating), y = mean_imdb_rating)) + 
  geom_bar(stat="identity", color='red',fill="#112446") +
  coord_flip() 
p

library(ggridges)
ridge <- matched %>% group_by(author) %>% mutate(n=n()) %>% filter(n>5)
ggplot(ridge, aes(x = averageRating, y = author, fill = author)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


#genre per author

genres_author <- matched %>% group_by(author) %>%summarise(n=n(),
                                                  perc_pick=mean(criticpick),
                                                  perc_rating <- mean(averageRating, na.rm=TRUE),
                                                  perc_Documentary=mean(Documentary),
                                                  perc_Drama=mean(Drama),
                                                  perc_Animation=mean(Animation),
                                                  perc_Comedy=mean(Comedy),
                                                  perc_Crime=mean(Crime),
                                                  perc_Action=mean(Action),
                                                  perc_Adventure=mean(Adventure),
                                                  perc_Biography=mean(Biography),
                                                  perc_Horror=mean(Horror),
                                                  perc_Mystery=mean(Mystery),
                                                  perc_Thriller=mean(Thriller),
                                                  perc_SciFi=mean(SciFi),
                                                  perc_Fantasy=mean(Fantasy),
                                                  perc_Family=mean(Family),
                                                  perc_Musical=mean(Musical),
                                                  perc_History=mean(History),
                                                  perc_Music=mean(Music),
                                                  perc_Romance=mean(Romance)) %>% 
  filter(n>9)
na.omit(genres_author)

library(dplyr)

tshek <- genres_author %>% 
  mutate(maxx = pmax(perc_Documentary, perc_Drama,perc_Comedy, perc_Crime,
                    perc_Action,perc_Adventure,perc_Biography,perc_Horror,
                    perc_Mystery,perc_Animation,perc_Thriller,perc_SciFi  ,   
                    perc_Fantasy,perc_Family,perc_Musical ,perc_History,
                    perc_Music,perc_Romance))

yshek2 <- genres_author %>%
  dplyr::select(perc_Documentary, perc_Drama,perc_Comedy, perc_Crime,
         perc_Action,perc_Adventure,perc_Biography,perc_Horror,
         perc_Mystery,perc_Animation,perc_Thriller,perc_SciFi  ,   
         perc_Fantasy,perc_Family,perc_Musical ,perc_History,
         perc_Music,perc_Romance) %>% 
  rowwise %>%
  mutate(Max = names(.)[which.max(c(perc_Documentary, perc_Drama,perc_Comedy, perc_Crime,
                                     perc_Action,perc_Adventure,perc_Biography,perc_Horror,
                                     perc_Mystery,perc_Animation,perc_Thriller,perc_SciFi  ,   
                                     perc_Fantasy,perc_Family,perc_Musical ,perc_History,
                                     perc_Music,perc_Romance))]) %>%  ungroup



#critics pick ratings vs non critics pick
matched %>% group_by(criticpick) %>% summarise(avg_imdb_rating=mean(averageRating, na.rm=TRUE))
matched$criticpick_f=as.factor(matched$criticpick)
ggplot(matched, aes(x = averageRating, y = criticpick_f, fill = criticpick_f)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


  



     