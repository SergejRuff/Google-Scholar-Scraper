# Function to count the number of elements with a specific class on the current page
count_elements <- function(page) {
  # Select elements with the specified class on the page
  elements <- rvest::html_text(rvest::html_elements(page, ".gs_rt"))
  # Count the number of selected elements
  print(elements)
  num_elements <- length(elements)
  return(num_elements)
}
