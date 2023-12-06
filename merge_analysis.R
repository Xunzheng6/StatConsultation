library(tidyverse)

# appending the data
merged_all <- rbind(Movie2022, Movie2023, Movie2023_H1,Movie2023_H1_p2, Movie2023_H2)

# keeping only movies using the /movies/ pattern in response.docs.web_url
merged <- merged_all %>%
  filter(str_detect(response.docs.web_url, "/movies"))
# we have 2013 movies, now keep only unique ones
#merged_unique <- unique(merged)
merged_unique <- merged %>% distinct(response.docs.snippet, .keep_all = TRUE)
#only 9 unique!

# to know what type of movie, we can find this information in the response.docs.keywords list

test2 <- merged_unique1$response.docs.keywords
#there is an inconsistency with where the subject is in the list, so I need a better way to extract that information
test2[[c(3,2)]][2]

