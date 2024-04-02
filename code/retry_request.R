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
