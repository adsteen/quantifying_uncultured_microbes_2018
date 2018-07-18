# Function to classify unclassified sequences as "unclassified"
as_unclassified <- function(x, unclass.label = "unclassified") {
  
  x <- if_else(is.na(x), "unclassified", x)
  x
}
