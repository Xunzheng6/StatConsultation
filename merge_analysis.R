library(tidyverse)
library(lubridate)
library(zoo)

# appending the data
Movie2018H1 <- load("Movie2018H1.Rdata")
Movie2018H1 <- MovieFile
Movie2018H2 <- load("Movie2018H2.Rdata")
Movie2018H2 <- MovieFile

Movie2019H1 <- load("Movie2019H1.Rdata")
Movie2019H1 <- MovieFile
Movie2019H2 <- load("Movie2019H2.Rdata")
Movie2019H2 <- MovieFile

Movie2020H1 <- load("Movie2020H1.Rdata")
Movie2020H1 <- MovieFile
Movie2020H2 <- load("Movie2020H2.Rdata")
Movie2020H2 <- MovieFile

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

# to know what type of movie, we can find this information in the response.docs.keywords list
typelist <- merged_unique$response.docs.keywords
merged_unique <- merged_unique %>% mutate(type=0)
for (i in 1:nrow(merged_unique)) {
merged_unique$type[i]=typelist[[c(i,2)]][1]
}
table(merged_unique$type)
check <- merged_unique %>% filter (merged_unique$type!="Movies" & merged_unique$type!="Documentary Films and Programs") 
#THIS METHOD LEAVES 98 UNCLASSIFIED. WE CAN DO BETTER


merged_unique <- merged_unique %>% mutate(type=0)
for (i in 1:nrow(merged_unique)) {
  typetable=as.data.frame(merged_unique$response.docs.keywords[i])
  merged_unique$type[i]=ifelse(any(typetable$value=="Documentary Films and Programs"),"Documentary Films and Programs",
                               ifelse(any(typetable$value=="Movies"),"Movies",
                                      ifelse(any(typetable$value=="Animated Films"),"Animated Films",NA)))
}
check2 <- merged_unique %>% filter (is.na(merged_unique$type))

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

# trend number of movie reviews
unique <- merged_unique %>% select(response.docs.pub_date,month, year, monthyear, response.docs.news_desk, type, author, criticpick)
trend1 <- unique %>% group_by(monthyear) %>% mutate(n_month=n()) %>% ungroup() %>% group_by(year) %>% 
  mutate(n_year=n()) %>% ungroup() %>% group_by(monthyear) %>% arrange(monthyear)
trend1$monthyear_c <- as.character(trend1$monthyear)

esquisse::esquisser()

trend1 %>%
  arrange(monthyear) %>% 
  group_by(monthyear, year) %>% 
  summarise(n_month=n()) %>% 
  ggplot() +
  aes(x = monthyear, y = n_month) +
  geom_line()+
  scale_color_gradient() +
  theme_minimal() 

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

covid <- rbind(covid2020p1,covid2020p2,
               covid2020p3,covid2020p4,
               covid2020p5,covid2020p6,
               covid2020p7,covid2020p8,
               covid2020p9,covid2020p10,
               covid2020p11,covid2020p12,
               covid2020p13) #13114
covid_unique <- covid %>% distinct(response.docs.snippet, .keep_all = TRUE) #18437 unique

trend2 <- covid_unique %>% mutate(month = month(response.docs.pub_date), 
                                  year = year(response.docs.pub_date))
trend2$monthyear <- as.yearmon(paste(trend2$year, trend2$month), "%Y %m")

# trend covid articles
trend2 <- trend2 %>% select(response.docs.pub_date,month, year, monthyear, response.docs.news_desk)
trend2 <- trend2 %>% group_by(monthyear) %>% mutate(n_month=n()) %>% ungroup() %>% group_by(year) %>% 
  mutate(n_year=n()) %>% ungroup() %>% group_by(monthyear) %>% arrange(monthyear)
trend2$monthyear_c <- as.character(trend2$monthyear)

trend2 %>%
  arrange(monthyear) %>% 
  group_by(monthyear, year) %>% 
  summarise(n_month=n()) %>% 
  ggplot() +
  aes(x = monthyear, y = n_month) +
  geom_line()+
  scale_color_gradient() +
  theme_minimal() 



