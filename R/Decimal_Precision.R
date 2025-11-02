decimal_precision <- function(x) {
  if (!is.numeric(x)) stop("Kolumna nie jest numeryczna.")
  x <- na.omit(x)
  if (length(x) == 0) return(0)
  
  dec <- sapply(strsplit(sub("0+$", "", sub(".*\\.", "", as.character(x))), ""), length)
  max(dec)
}