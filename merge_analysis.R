library(tidyverse)

# appending the data
merged_all <- rbind(Movie2021H1, Movie2021H2, Movie2022H1, Movie2022H2, Movie2023H1, Movie2023H2)

# keeping only movies using the /movies/ pattern in response.docs.web_url
merged <- merged_all %>%
  filter(str_detect(response.docs.web_url, "/movies"))
# we have 2229 movies, now keep only unique ones
#merged_unique <- unique(merged)
merged_unique <- merged %>% distinct(response.docs.snippet, .keep_all = TRUE)
#only 2013 unique!

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
  merged_unique$type[i]=ifelse(any(typetable$value=="Documentary Films and Programs"),"Documentary Films and Programs",ifelse(any(typetable$value=="Movies"),"Movies",NA))
}
check2 <- merged_unique %>% filter (is.na(merged_unique$type))


sorted_typelist <- typelist[order(sapply(typelist,'[[',1))]
sorted_li
#there is an inconsistency with where the subject is in the list, so I need a better way to extract that information
test2[[c(1,2)]][1]

