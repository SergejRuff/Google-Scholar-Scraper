rm(list=ls())

## Rselium install
## You need: Java (JDK) -> Azul JDK works better with other packages in R like tabularize (java 8).
## https://www.azul.com/downloads/?package=jdk#zulu -> ubuntu: sudo apt install ./<package>.deb
## install.packages("Rselenium")
##  You need chromedriver and need to know your chromeversion(type chrome://version/ into chrome )
##  binman::list_versions("chromedriver") shows your available drivers.
##  make sure that teh two first digets match
##  driver: https://googlechromelabs.github.io/chrome-for-testing/
##  Firefox uses geckodriver -> snap version of firefox doesnt work
##  instead use sudo snap remove firefox
##  sudo add-apt-repository ppa:mozillateam/ppa
##  echo '
##  Package: *
##  Pin: release o=LP-PPA-mozillateam
##  Pin-Priority: 1001
##  ' | sudo tee /etc/apt/preferences.d/mozilla-firefox
##
##  echo 'Unattended-Upgrade::Allowed-Origins:: "LP-PPA-mozillateam:${distro_codename}";' | sudo tee /etc/apt/apt.conf.d/51unattended-upgrades-firefox
##  sudo apt install firefox
##  Never forget to remove the LINCE for teh Chrome Driver. Those cause errors
##  video instruction: https://www.youtube.com/watch?v=GnpJujF9dBw&t=130s
##
library(RSelenium)
library(tidyverse)
library(port4me) # allows to find/generate free ports
library(wdman)
library(rvest)
library(stringr)

# download all drivers that we need
selenium(retcommand=T) # retcommand=T

# show where every driver gets intalled to: important for driver
selenium_object <- selenium(retcommand=T,check = F)

# shows which driver versions are supported
#binman::list_versions("chromedriver")
binman::list_versions("geckodriver")

# start server -> start Seleniumserver
rD<- rsDriver(browser = "firefox",
                             geckover = "0.34.0",
                             verbose = FALSE, # verbose supresses messages while booting up
                             port =port4me::port4me()) # port4me

# Get the remote driver (remDr) object
remDr <- rD [["client"]]

# Define your search terms
search_terms <- "miR-196-b"

# Function to extract data from a page
extract_data <- function(page_source) {
  page <- read_html(page_source)
  titles <- page %>% html_nodes(".gs_rt") %>% html_text()
  authors <- page %>% html_nodes(".gs_a") %>% html_text()
  years <- str_extract(authors, "\\d{4}")
  authors <- str_replace(authors, "\\d{4}", "")
  urls <- page %>% html_nodes(".gs_rt a") %>% html_attr("href")
  cited_by <- page %>% html_nodes(".gs_fl a:nth-child(3)") %>% html_text()
  cited_by <- as.integer(str_extract(cited_by, "\\d+"))

  data.frame(Title_name = titles, Author_Names = authors, Year_Publication = years, Title_URL = urls, cited_by = cited_by)
}

# Function to search for a specific term on Google Scholar
search_google_scholar <- function(term) {
  tryCatch({
    remDr$navigate("https://scholar.google.com/")
    search_box <- remDr$findElement("css", "#gs_hdr_tsi")
    search_box$sendKeysToElement(list(term, key="enter"))
    Sys.sleep(5) # Allow time for page to load

    pages <- 2 # Number of pages to scrape
    results <- data.frame()

    for (page in 1:pages) {
      page_source <- remDr$getPageSource()[[1]]
      page_data <- extract_data(page_source)
      results <- rbind(results, page_data)

      next_button <- remDr$findElement("css", "#gs_n a")
      if (length(next_button) == 0) {
        break
      } else {
        next_button$clickElement()
        Sys.sleep(5) # Allow time for page to load
      }
    }

    return(results)
  }, error = function(e) {
    message("An error occurred: ", conditionMessage(e))
    NULL
  })
}

# Execute the search and scrape the data
search_results <- search_google_scholar(search_terms)

# Close the browser
remDr$close()

# Stop the Selenium server
rD$server$stop()

