library(tidyverse)
library(lubridate)
library(zoo)

# appending the data
merged_all <- rbind(Movie2021H1, Movie2021H2, Movie2022H1, Movie2022H2, Movie2023H1, Movie2023H2)

# keeping only movies using the /movies/ pattern in response.docs.web_url
merged <- merged_all %>%
  filter(str_detect(response.docs.web_url, "/movies"))
# we have 2229 movies, now keep only unique ones
#merged_unique <- unique(merged)
merged_unique <- merged %>% distinct(response.docs.snippet, .keep_all = TRUE)
#only 2103 unique!

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








