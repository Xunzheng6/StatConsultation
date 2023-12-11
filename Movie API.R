library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)


NYTIMES_KEY <- "hCt4EAjrvjsBswpEqfukn2lLVCkpCgCG" ###need individual access

movie = "Movies" ## section_name
review = "Review" ##type_of_content
begin_date1 = as.POSIXlt(Sys.time()-86400*7)
begin_date2 = paste0(substr(begin_date1,0,4),substr(begin_date1,6,7),substr(begin_date1,9,10))
end_date1 = as.POSIXlt(Sys.time()-86400)
end_date2 = paste0(substr(end_date1,0,4),substr(end_date1,6,7),substr(end_date1,9,10))
covid <- "covid" ## query

baseur1 <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name:",movie,
                  "AND type_of_material:",review, "&sort=newest",
                  "&begin_date2=",begin_date,"&end_date2=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY)

##https://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name:"Movies" AND type_of_material:"Review"&sort=newest&page=0&api-key{NYTIMES_KEY}

initialQuery <- fromJSON(baseurl2)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages_2023 <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl2, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2023[[i+1]] <- nytSearch 
  Sys.sleep(12) 
}

MovieFile <- rbind_pages(pages_2023)
save(MovieFile,file=paste0(end_date2,".Rdata"))


