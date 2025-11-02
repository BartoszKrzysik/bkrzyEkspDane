decimal_precision <- function(x) {
  if (!is.numeric(x)) stop("Kolumna nie jest numeryczna.")
  x <- na.omit(x)
  if (length(x) == 0) return(0)
  
  dec <- sapply(strsplit(sub("0+$", "", sub(".*\\.", "", as.character(x))), ""), length)
  max(dec)
}

compute_binwidth <- function(x, max_unique = 30) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)

  unique_vals <- length(unique(x))

  if (unique_vals <= max_unique) {
    binwidth <- 1
  } else {
    iqr_val <- IQR(x)
    n_obs <- length(x)
    binwidth <- 2 * iqr_val / (n_obs)^(1/2)

    if (binwidth <= 0 || is.na(binwidth)) {
      binwidth <- diff(range(x)) / 30
    }

    binwidth <- signif_half_up(binwidth, digits = decimal_precision(x))
  }

  return(binwidth)
}
