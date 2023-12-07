install.packages("tidyverse")
install.packages("jsonlite")
install.packages("lubridate")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")

library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)


NYTIMES_KEY <- "b7AEYMJYY111XfaykaXUmGlAM0CCzyzm" ###need individual access


movie <- "Movies" ## section_name
review = "Review" ##type_of_content
<<<<<<< HEAD
begin_date <- "20230101"
end_date <- "20230103"
=======
begin_date <- "20210101"
end_date <- "20211231"
>>>>>>> 191c83bc7626a7cb95a5f6b487b62fec46e59029

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name:",movie,
                  "AND type_of_material:",review, "&sort=newest&page=0",
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

##https://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name%3A"Movies" AND type_of_material%3A"Review"&sort=newest&page=0&api-key{NYTIMES_KEY}

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages_2023 <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2023[[i+1]] <- nytSearch 
  Sys.sleep(12) 
}

##2023H1 page=83

Movie2021 <- rbind_pages(pages_2023)
save(Movie2021,file="Movie2021.Rdata") ##need to repull

table(Movie2023_H2)







