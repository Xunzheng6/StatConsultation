library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)


NYTIMES_KEY <- "RF0Jo5G7GNH5GoIwgMXWGzn9fhyMP8xs" ###need individual access
## YCQXcIE3FWEolFlu6ouiSaGUwpo5iG1d  
## tGHvuXnYKOxwJYoDyjkrcD8kJP4CYQ4D  
## hCt4EAjrvjsBswpEqfukn2lLVCkpCgCG  
## RF0Jo5G7GNH5GoIwgMXWGzn9fhyMP8xs

movie <- "Movies" ## section_name
review = "Review" ##type_of_content
begin_date <- "20230323"
end_date <- "20231010"  
covid <- "covid" ## query

baseur1 <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name:",movie,
                  "AND type_of_material:",review, "&sort=newest",
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY)

baseurl2 <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",covid,
                  "&sort=newest","&begin_date=",begin_date,"&end_date=",end_date,
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


CovidFile <- rbind_pages(pages_2023)
save(CovidFile,file=paste0(end_date,"Covid.Rdata")) ##Save data as separate file, need to change data name




