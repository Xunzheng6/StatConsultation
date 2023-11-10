library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)


NYTIMES_KEY <- "tGHvuXnYKOxwJYoDyjkrcD8kJP4CYQ4D" ###need individual access

movie <- "Movies" ## section_name
review = "Review" ##type_of_content
begin_date <- "20230101"
end_date <- "20230903"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name%3A",movie,
                  "AND type_of_material%3A",review, "&sort=newest&page=0",
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

##https://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name%3A"Movies" AND type_of_material%3A"Review"&sort=newest&page=0&api-key{NYTIMES_KEY}

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages_2023 <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2014[[i+1]] <- nytSearch 
  Sys.sleep(12) 
}

Movie2023 <- rbind_pages(pages_2014)
save(Movie2023,file="Movie2023.Rdata") ##need to repull






