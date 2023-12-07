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

test2 <- merged_unique1$response.docs.keywords

test2 <- merged_unique$response.docs.keywords
#there is an inconsistency with where the subject is in the list, so I need a better way to extract that information
test2[[c(3,2)]][2]
