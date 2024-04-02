library(RSelenium)
rm(list=ls())

## This version of the function doesnt require the user to sepcify number of pages he wants to scrape.
## it scrapes all pages.
## might take longer, might cause http 429 error
## proxies taken from: https://scrapingant.com/free-proxies/

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
      # Initialize RSelenium
      driver <- rsDriver(browser = "firefox", port = 4445L)
      remote_driver <- driver[["client"]]

      # Set proxy for the session
      proxy_info <- paste0(proxy_ip, ":", proxy_port)
      remote_driver$addProxy(type = "manual", httpProxy = proxy_info, httpsProxy = proxy_info)

      # Navigate to the URL
      remote_driver$navigate(url)

      # If the connection is successful, return the URL and port of the proxy
      return(list(url = proxy_ip, port = proxy_port))
    }, error = function(e) {
      cat("Error occurred with proxy", current_proxy, ":", conditionMessage(e), "\n")
      # Close the driver if an error occurs
      driver$server$stop()
    })
  }
  cat("All proxies failed. Failed to establish connection.\n")
  return(NULL)
}

scrape_gs <- function(term, proxies, ...) {
  library(rvest)
  library(RSelenium)
  library(stringr)
  library(Randomuseragent)

  gs_url_base <- "https://scholar.google.com/scholar?q="
  term <- gsub("^mmu-", "", term)

  print(paste0("intitle:", term))

  best_proxy <- retry_request(paste0(gs_url_base, URLencode(paste0("intitle:", term))), proxies)

  # Initialize RSelenium
  driver <- rsDriver(browser = "firefox", port = 4445L)
  remote_driver <- driver[["client"]]

  # Set proxy for the session
  proxy_info <- paste0(best_proxy$url, ":", best_proxy$port)
  remote_driver$addProxy(type = "manual", httpProxy = proxy_info, httpsProxy = proxy_info)

  # Navigate to the URL
  remote_driver$navigate(paste0(gs_url_base, URLencode(paste0("intitle:", term))))

  # Extract page content
  page_html <- remote_driver$getPageSource()[[1]]
  page <- read_html(page_html, encoding = "UTF-8")

  # Extract the entire page content as text
  page_text <- as.character(page)

  # Extract numbers within <span> tags from the text
  numbers <- str_extract_all(page_text, "span>(\\d+)") %>%
    unlist() %>%
    str_replace("span>", "") %>%
    as.integer()

  # Find the maximum number
  max_number <- max(numbers, na.rm = TRUE)

  if (is.infinite(max_number)) {
    cat("Max number is -Inf. Trying with a new proxy.\n")
    rotate_proxy()  # Rotate to a new proxy
    return(scrape_gs(term, proxies))  # Retry scraping with a new proxy
  }

  cat(paste0(paste0("intitle:", term), " has ", max_number, " pages with results\n"))

  result_list <- list()
  i <- 1

  for (n_page in 0:(max_number - 1) * 10) {
    gs_url <- paste0(gs_url_base, "&start=", n_page, "&q=", noquote(gsub("\\s+", "+", trimws(paste0("intitle:", term)))))
    cat(paste(gs_url, "-> page:", n_page / 10 + 1), "\n")

    t0 <- Sys.time()
    # Navigate to the next page
    remote_driver$navigate(gs_url)

    # Extract page content
    page_html <- remote_driver$getPageSource()[[1]]
    wbpage <- read_html(page_html, encoding = "UTF-8")

    t1 <- Sys.time()
    response_delay <- as.numeric(t1 - t0)  # backing off time
    crawl_delay <- 3

    # Avoid HTTP error 429 due to too many requests - use crawl delay & back off
    Sys.sleep(crawl_delay + 3 * response_delay + runif(n = 1, min = 0.5, max = 1))

    if ((i %% 10) == 0) {  # sleep every 10 iterations
      message("taking a break")
      Sys.sleep(10 + 10 * response_delay + runif(n = 1, min = 0, max = 1))
    }
    i <- i + 1

    # Raw data
    titles <- html_text(html_nodes(wbpage, ".gs_rt"))
    authors_years <- html_text(html_nodes(wbpage, ".gs_a"))
    part_abstracts <- html_text(html_nodes(wbpage, ".gs_rs"))
    bottom_row_nodes <- html_nodes(wbpage, ".gs_fl")
    bottom_row_nodes <- bottom_row_nodes[!grepl("gs_ggs gs_fl", as.character(bottom_row_nodes), fixed = TRUE)] # exclude the ones with this tag, they are download links
    bottom_row <- html_text(bottom_row_nodes)

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
          page = n_page / 10 + 1,
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
  result_df <- result_df[!grepl("\\b(?:[[:alpha:]]*omas?|cancer|leukemia)\\b", result_df$title, ignore.case = TRUE), ]

  # Stop RSelenium driver
  remote_driver$close()

  return(result_df)
}

test <- scrape_gs("mmu-miR-196b-5p",proxies)
