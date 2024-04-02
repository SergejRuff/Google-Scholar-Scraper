rm(list=ls())

library(rvest)
library(httr)
library(magrittr)
library(stringr)
library(Randomuseragent)
library(RSelenium)

## This version of the function doesnt require the user to sepcify number of pages he wants to scrape.
## it scrapes all pages.
## might take longer, might cause http 429 error
## proxies taken from: https://scrapingant.com/free-proxies/
# this function goes through only the first 10 pages
# after taking a break count_elements returns 0 links causing empty lists

proxies <- c(
  "72.10.160.90:2695",
  "138.219.249.5:3128",
  "158.181.204.159:8080",
  "44.226.167.102:80",
  "162.19.7.57:41115",
  "67.213.210.61:26367",
  "67.213.212.58:51494",
  "67.213.210.61:26367",
  "67.213.212.58:51494",
  "195.154.43.86:41207",
  "67.43.228.253:12319",
  "138.201.21.227:41601",
  "67.43.228.253:22975",
  "67.43.228.253:21607",
  "114.132.202.246:8080",
  "67.43.236.20:24489",
  "54.213.136.83:52167",
  "67.43.236.20:5051",
  "67.43.236.20:5051",
  "67.43.228.253:26107",
  "67.43.227.227:28963",
  "67.43.228.251:2305",
  "72.10.164.178:20313",
  "67.43.228.253:28539",
  "72.10.164.178:8093",
  "88.250.88.246:5314",
  "72.10.164.178:7371",
  "103.139.25.121:8080",
  "95.217.187.170:47102",
  "72.10.160.170:6407",
  "190.144.34.146:3128",
  "209.126.104.38:12457",
  "67.43.227.227:30759",
  "67.43.236.20:24163",
  "89.187.162.70:8443",
  "102.36.217.30:8080",
  "67.213.212.39:64185",
  "72.10.164.178:21929",
  "45.11.95.165:5044",
  "18.135.133.116:3128",
  "72.10.164.178:3783",
  "67.43.227.228:31315",
  "129.151.72.85:80",
  "54.186.85.186:11051",
  "103.247.120.46:10800",
  "67.213.212.39:64185",
  "67.43.228.253:2431",
  "67.43.227.228:4309",
  "162.214.227.68:37597",
  "72.10.164.178:10183",
  "162.19.7.61:15843",
  "162.19.7.61:15843",
  "67.43.228.253:6749",
  "138.43.98.203:82",
  "67.43.227.227:31325",
  "58.220.213.103:80",
  "177.130.140.214:3128",
  "72.10.164.178:22703",
  "35.154.71.72:1080",
  "212.83.143.204:42639",
  "212.83.143.204:42639",
  "103.18.76.134:8080",
  "78.186.58.54:1455",
  "161.97.173.78:29423",
  "20.247.228.80:80",
  "94.23.220.136:48812",
  "109.238.12.156:47537",
  "67.43.228.251:6785",
  "173.212.237.43:56159",
  "47.243.30.66:35305"
)

# Function to rotate proxies
rotate_proxy <- function() {
  current_proxy <<- sample(proxies, 1)
}


retry_request <- function(url, proxies) {
  max_proxies <- length(proxies)
  shuffled_proxies <- sample(proxies)

  for (proxy_index in 1:max_proxies) {
    current_proxy <- shuffled_proxies[proxy_index]


    # Split current_proxy into IP address and port
    proxy_parts <- unlist(strsplit(current_proxy, ":"))
    proxy_ip <- proxy_parts[1]
    proxy_port <- as.numeric(proxy_parts[2])

    tryCatch({
      session <- rvest::session(url, httr::use_proxy(url = proxy_ip, port = proxy_port))
      # If the connection is successful, return the URL and port of the proxy
      return(list(url = proxy_ip, port = proxy_port))
    }, error = function(e) {
      cat("Error occurred with proxy", current_proxy, ":", conditionMessage(e), "\n")
    })
  }
  cat("All proxies failed. Failed to establish connection.\n")
  return(NULL)
}

# Function to count the number of elements with a specific class on the current page
count_elements <- function(page) {
  # Select elements with the specified class on the page
  elements <- rvest::html_text(rvest::html_elements(page, ".gs_rt"))
  # Count the number of selected elements
  num_elements <- length(elements)
  return(num_elements)
}


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





