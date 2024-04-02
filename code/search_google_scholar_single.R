rm(list=ls())
library(rvest)

search_query <- "mmu-miR-196b-5p"


search_url <- paste0("https://scholar.google.com/scholar?q=", URLencode(search_query))
search_page <- read_html(search_url)



titles <- search_page %>%
  html_nodes(".gs_rt") %>%
  html_text()

years <- search_page %>%
  html_nodes(".gs_a") %>%
  html_text()
years <- gsub("^.*(\\d{4}).*", "\\1", years, perl = TRUE)
# Combine titles and years into a data frame
search_results <- data.frame(Title = titles, Year = years)



# Filter out titles containing words ending with "oma" or "omas", as well as "cancer" and "leukemia"
search_results <- search_results[!grepl("\\b(?:[[:alpha:]]*omas?|cancer|leukemia)\\b", search_results$Title, ignore.case = TRUE), ]

# Print titles that are not filtered out
print(search_results$Title)
