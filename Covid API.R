library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)


NYTIMES_KEY <- "YCQXcIE3FWEolFlu6ouiSaGUwpo5iG1d" ###need individual access

begin_date1 = as.POSIXlt(Sys.time()-86400*7)
begin_date2 = paste0(substr(begin_date1,0,4),substr(begin_date1,6,7),substr(begin_date1,9,10))
end_date1 = as.POSIXlt(Sys.time()-86400)
end_date2 = paste0(substr(end_date1,0,4),substr(end_date1,6,7),substr(end_date1,9,10))
covid <- "covid" ## query

baseurl2 <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",covid,
                   "&sort=newest","&begin_date=",begin_date2,"&end_date=",end_date2,
                   "&facet_filter=true&api-key=",NYTIMES_KEY)

initialQuery <- fromJSON(baseurl2)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages_2023 <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl2, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2023[[i+1]] <- nytSearch 
  Sys.sleep(12) 
}


CovidFile <- rbind_pages(pages_2023)
save(CovidFile,file=paste0(end_date,"Covid.Rdata")) ##Save data as separate file, need to change data name
