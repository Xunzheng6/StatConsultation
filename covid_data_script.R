################
# loading API covid data
###############

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
covid2020p7 <- covid2020p7 %>% dplyr::select(-response.docs.slideshow_credits)
covid2020p8 <- load("Covid2020P8.Rdata")
covid2020p8 <- CovidFile
covid2020p8 <- covid2020p8 %>% dplyr::select(-response.docs.slideshow_credits)
covid2020p9 <- load("Covid2020P9.Rdata")
covid2020p9 <- CovidFile
covid2020p9 <- covid2020p9 %>% dplyr::select(-response.docs.slideshow_credits)
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
covid2021p2 <- covid2021p2 %>% dplyr::select(-response.docs.slideshow_credits)
covid2021p3 <- CovidFile
load("20210720Covid.Rdata")
covid2021p4 <- CovidFile
covid2021p4 <- covid2021p4 %>% dplyr::select(-response.docs.slideshow_credits)
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
trend2 <- trend2 %>% dplyr::select(response.docs.pub_date,month, year, monthyear, response.docs.news_desk)
trend2 <- trend2 %>% group_by(monthyear) %>% mutate(n_month=n()) %>% ungroup() %>% group_by(year) %>% 
  mutate(n_year=n()) %>% ungroup() %>% group_by(monthyear) %>% arrange(monthyear)
trend2$monthyear_c <- as.character(trend2$monthyear)



##REMOVE THE FOLLOWING COVID DATA DAYS:
# 2021-01-27
# 2023-03-22



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