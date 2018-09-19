library(tidyverse)
library(stringr)

domain <-
  function(x) {
    strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
  }

url_pattern <-
  "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

"C:/Users/abreza/Downloads/Wikipedia-20180919171309.xml" %>% 
  readLines() %>% 
  str_extract_all(url_pattern) %>% 
  unlist() %>% 
  strsplit(split = "/http://", fixed = TRUE) %>% 
  unlist() %>% 
  str_replace_all(pattern = "&lt;/ref&gt;", replacement = "") %>% 
  str_replace_all(pattern = "www2.", replacement = "") %>% 
  str_replace_all(pattern = "www3.", replacement = "") %>% 
  na.omit() %>% 
  map_chr(domain) %>% 
  as_tibble() %>% 
  setNames("domain") %>% 
  filter(!(domain %in% c("mediawiki.org", "w3.org", "en.wikipedia.org"))) %>% 
  group_by(domain) %>% 
  summarise(n = n()) %>% 
  filter(n > 3) %>% 
  group_by(domain) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n))

write_csv(wiki, "wiki_domains.csv")
