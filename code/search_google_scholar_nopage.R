rm(list=ls())

library(rvest)
library(httr)
library(magrittr)
library(stringr)
library(Randomuseragent)
library(RSelenium)
source("code/proxies.R")
source("code/rotate_proxies.R")
source("code/retry_request.R")
source("code/count_elements.R")

## This version of the function doesnt require the user to sepcify number of pages he wants to scrape.
## it scrapes all pages.
## might take longer, might cause http 429 error
## proxies taken from: https://scrapingant.com/free-proxies/
# this function goes through only the first 10 pages
# after taking a break count_elements returns 0 links causing empty lists


scrape_gs <- function(term,proxies,...) {






  gs_url_base <- "https://scholar.google.com/scholar?q="
  term <- gsub("^mmu-", "", term)
  #term <- paste0("Virus AND intitle:",term)
  print(paste0("intitle:",term))

  best_proxy <- retry_request(paste0(gs_url_base, URLencode(paste0("intitle:",term))), proxies)
  gs_page <- rvest::session(paste0(gs_url_base, URLencode(paste0("intitle:",term))), httr::use_proxy(url = best_proxy[[1]], port = best_proxy[[2]]))
  #gs_page <- rvest::session(paste0(gs_url_base, URLencode(term)), httr::use_proxy(url = proxy_ip, port = proxy_port))

  cat(paste("current link ->",paste0(gs_url_base, URLencode(paste0("intitle:",term)))),"\n")

  # Read the HTML content of the Google Scholar search results page
  page <- read_html(gs_page,user_agent=Randomuseragent::random_useragent())

  # Count the number of links on the page
  num_links <- count_elements(page)
  message(num_links)
  if (num_links == 0) {
    return(NULL)
  }

  # Extract the entire page content as text
  page_text <- as.character(page)
  #print(page_text)

  # Extract numbers within <span> tags from the text
  numbers <- str_extract_all(page_text, "span>(\\d+)") %>%
    unlist() %>%
    str_replace("span>", "") %>%
    as.integer()

  # Find the maximum number
  # Check if the count of elements is less than 10
  if (num_links < 10) {
    message("search-term has only 1 page")
    max_number <- 1
  } else {
    max_number <- max(numbers, na.rm = TRUE)
  }

  if (is.infinite(max_number)) {
    cat("Max number is -Inf. Trying with a new proxy.\n")
    rotate_proxy()  # Rotate to a new proxy
    return(scrape_gs(term, proxies))  # Retry scraping with a new proxy
  }
  cat(paste0(paste0("intitle:",term)," has ",max_number, " pages with results\n"))


  # set httr config outside of function and use them inside ...; e.g.:
  # useragent <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") # spoof user agent
  # proxy <- httr::use_proxy(url = "proxy.com", port = 8080, username = "dave", password = "pass", auth = "basic")

  result_list <- list()
  i <- 1

  for (n_page in 0:(max_number- 1)*10) {  # gs page indexing starts with 0; there are 10 articles per page, see "?start=" param
    gs_url <- paste0(gs_url_base, "&start=", n_page, "&q=", noquote(gsub("\\s+", "+", trimws(paste0("intitle:",term)))))
    cat(paste(gs_url,"-> page:",n_page/10 + 1),"\n")

    t0 <- Sys.time()
    session <- rvest::session(gs_url, httr::use_proxy(url = best_proxy[[1]], port = best_proxy[[2]]))
    #cat(paste0("current http status is ",session$response$status_code))
    t1 <- Sys.time()
    response_delay <- as.numeric(t1-t0)  # backing off time
    wbpage <- rvest::read_html(session,user_agent=Randomuseragent::random_useragent())

    #crawl_delay <- sample(2:10, 1)
    crawl_delay <- 3


    # Avoid HTTP error 429 due to too many requests - use crawl delay & back off
    Sys.sleep(crawl_delay + 3*response_delay + runif(n = 1, min = 0.5, max = 1))
    if((i %% 10) == 0) {  # sleep every 10 iterations
      message("taking a break")
      Sys.sleep(10 + 10*response_delay + runif(n = 1, min = 0, max = 1))
    }
    i <- i + 1

    # Raw data
    titles <- rvest::html_text(rvest::html_elements(wbpage, ".gs_rt"))
    #print(titles)
    authors_years <- rvest::html_text(rvest::html_elements(wbpage, ".gs_a"))
    part_abstracts <- rvest::html_text(rvest::html_elements(wbpage, ".gs_rs"))
    bottom_row_nodes <- rvest::html_elements(wbpage, ".gs_fl")
    bottom_row_nodes <- bottom_row_nodes[!grepl("gs_ggs gs_fl", as.character(bottom_row_nodes), fixed = TRUE)] # exclude the ones with this tag, they are download links
    bottom_row <- rvest::html_text(bottom_row_nodes)

    # Processed data
    authors <- gsub("^(.*?)\\W+-\\W+.*", "\\1", authors_years, perl = TRUE)
    years <- gsub("^.*(\\d{4}).*", "\\1", authors_years, perl = TRUE)
    citations <- strsplit(gsub("(?!^)(?=[[:upper:]])", " ", bottom_row, perl = TRUE), "  ")  # split on capital letter to get Number of citations link
    citations <- lapply(citations, "[", 3)
    n_citations <- suppressWarnings(as.numeric(sub("\\D*(\\d+).*", "\\1", citations)))

    # Store in list
    result_list <- append(
      result_list,
      list(
        list(
          page = n_page/10 + 1,
          term = term,
          title = titles,
          authors = authors,
          year = years,
          n_citations = n_citations,
          abstract = part_abstracts
        )
      )
    )
  }

  # Return as data frame
  result_df <- lapply(result_list, as.data.frame)
  result_df <- as.data.frame(do.call(rbind, result_df))
  result_df<- result_df[!grepl("\\b(?:[[:alpha:]]*omas?|cancer|leukemia)\\b", result_df$title, ignore.case = TRUE), ]

  return(result_df)
}




testcsv <- read.csv("/home/sergej/Desktop/coding/viper/data/intercept/intersection_3.csv")




for (i in 24:length(testcsv$MiRNA)) {
  mirna_name <- testcsv$MiRNA[[i]]
  test <- scrape_gs(mirna_name, proxies)

  # Check if test is NULL, which indicates that no links were found
  if (is.null(test)) {
    cat("No links found for:", mirna_name, "\n")
    next  # Skip to the next iteration
  }

  output_file <- paste0("/home/sergej/Desktop/coding/scholar_scraper/output/", mirna_name, ".csv")
  write.csv(test, output_file, row.names = FALSE)
  cat("Scraped and saved:", mirna_name, "\n")
}





