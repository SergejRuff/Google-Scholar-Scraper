# Function to rotate proxies
rotate_proxy <- function() {
  current_proxy <<- sample(proxies, 1)
}
