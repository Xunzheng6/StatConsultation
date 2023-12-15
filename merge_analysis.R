library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)

setwd("~/Desktop/A3SR/STATS CONSULTING/StatConsultation/")

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
merged_unique$author <- merged_unique$response.docs.byline.original<-gsub("By","",as.character(merged_unique$response.docs.byline.original))

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
unique <- merged_unique %>% select(response.docs.pub_date,month, year, monthyear, 
                                   response.docs.news_desk, type, author, criticpick,
                                   movie_name, movie_name1, movie_name2)
unique <- unique %>% group_by(monthyear) %>% mutate(n_month=n()) %>% ungroup() %>% group_by(year) %>% 
  mutate(n_year=n()) %>% ungroup() %>% group_by(monthyear) %>% arrange(monthyear)


###------------------------------------------------------------------------------------------------------------###
# trend number of articles over time
covid2020p1 <- load("Covid2020P1.Rdata")
covid2020p1 <- CovidFile
covid2020p2 <- load("Covid2020P2.Rdata")
covid2020p2 <- CovidFile
covid2020p3 <- load("Covid2020P3.Rdata")
covid2020p3 <- CovidFile
covid2020p4 <- load("Covid2020P4.Rdata")
covid2020p4 <- CovidFile
covid2020p5 <- load("Covid2020P5.Rdata")
covid2020p5 <- CovidFile
covid2020p6 <- load("Covid2020P6.Rdata")
covid2020p6 <- CovidFile
covid2020p7 <- load("Covid2020P7.Rdata")
covid2020p7 <- CovidFile
covid2020p7 <- covid2020p7 %>% select(-response.docs.slideshow_credits)
covid2020p8 <- load("Covid2020P8.Rdata")
covid2020p8 <- CovidFile
covid2020p8 <- covid2020p8 %>% select(-response.docs.slideshow_credits)
covid2020p9 <- load("Covid2020P9.Rdata")
covid2020p9 <- CovidFile
covid2020p9 <- covid2020p9 %>% select(-response.docs.slideshow_credits)
covid2020p10 <- load("Covid2020P10.Rdata")
covid2020p10 <- CovidFile
#covid2020p10 <- covid2020p10 %>% select(-response.docs.slideshow_credits)
covid2020p11 <- load("Covid2020P11.Rdata")
covid2020p11 <- CovidFile
#covid2020p11 <- covid2020p11 %>% select(-response.docs.slideshow_credits)
covid2020p12 <- load("Covid2020P12.Rdata")
covid2020p12 <- CovidFile
#covid2020p12 <- covid2020p12 %>% select(-response.docs.slideshow_credits)
covid2020p13 <- load("Covid2020P13.Rdata")
covid2020p13 <- CovidFile
#covid2020p13 <- covid2020p13 %>% select(-response.docs.slideshow_credits)
covid2020p14 <- load("Covid2020P14.Rdata")
covid2020p14 <- CovidFile

load("20210228Covid2021P2.Rdata")
covid2021p1 <- CovidFile
load("20210413Covid.Rdata")
covid2021p2 <- CovidFile
load("20210525Covid.Rdata")
covid2021p2 <- covid2021p2 %>% select(-response.docs.slideshow_credits)
covid2021p3 <- CovidFile
load("20210720Covid.Rdata")
covid2021p4 <- CovidFile
covid2021p4 <- covid2021p4 %>% select(-response.docs.slideshow_credits)
load("20210910Covid.Rdata")
covid2021p5 <- CovidFile
load("20211105Covid1.Rdata")
covid2021p6 <- CovidFile
load("20211105Covid2.Rdata")
covid2021p7 <- CovidFile
load("20211130Covid.Rdata")
covid2021p8 <- CovidFile
load("20220120Covid1.Rdata")
covid2022p1 <- CovidFile
load("20220120Covid2.Rdata")
covid2022p2 <- CovidFile
load("20220325Covid.Rdata")
covid2022p3 <- CovidFile
load("20220620Covid.Rdata")
covid2022p4 <- CovidFile
load("20221031Covid.Rdata")
covid2022p5 <- CovidFile
load("20230320Covid.Rdata")
covid2023p1 <- CovidFile
load("20231010Covid.Rdata")
covid2023p2 <- CovidFile
load("20231210Covid.Rdata")
covid2023p3 <- CovidFile
load("20231210Covid2.Rdata")
covid2023p4 <- CovidFile


covid <- rbind(covid2020p1,covid2020p2,
               covid2020p3,covid2020p4,
               covid2020p5,covid2020p6,
               covid2020p7,covid2020p8,
               covid2020p9,covid2020p10,
               covid2020p11,covid2020p12,
               covid2020p13, covid2020p14,
               covid2021p1,covid2021p2,
               covid2021p3,covid2021p4,
               covid2021p5,covid2021p6,
               covid2021p7,covid2021p8,
               covid2022p1,covid2022p2,
               covid2022p3,covid2022p4,
               covid2022p5,
               covid2023p1,covid2023p2,
               covid2023p3,covid2023p4) #13114
covid_unique <- covid %>% distinct(response.docs.snippet, .keep_all = TRUE) #49401 of which 44003 are unique 

trend2 <- covid_unique %>% mutate(month = month(response.docs.pub_date), 
                                  year = year(response.docs.pub_date))
trend2$monthyear <- as.yearmon(paste(trend2$year, trend2$month), "%Y %m")

# trend covid articles
trend2 <- trend2 %>% select(response.docs.pub_date,month, year, monthyear, response.docs.news_desk)
trend2 <- trend2 %>% group_by(monthyear) %>% mutate(n_month=n()) %>% ungroup() %>% group_by(year) %>% 
  mutate(n_year=n()) %>% ungroup() %>% group_by(monthyear) %>% arrange(monthyear)
trend2$monthyear_c <- as.character(trend2$monthyear)

tt <- trend2 %>% filter(n_month>2000)



##REMOVE THE FOLLOWING COVID DATA DAYS:
# 2021-01-27
# 2023-03-22



##### PLOTS

#review trends
unique %>%
  arrange(monthyear) %>% 
  group_by(monthyear, year) %>% 
  summarise(n_month=n()) %>% 
  ggplot() +
  aes(x = monthyear, y = n_month) +
  geom_line()+
  scale_color_gradient() +
  theme_minimal() 

#covid trends
trend2 %>%
  arrange(monthyear) %>% 
  group_by(monthyear, year) %>% 
  summarise(n_month=n()) %>% 
  ggplot() +
  aes(x = monthyear, y = n_month) +
  geom_line()+
  scale_color_gradient() +
  theme_minimal() 





