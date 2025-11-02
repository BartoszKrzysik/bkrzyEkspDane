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
    binwidth <- 2 * iqr_val / (n_obs)^(1/3)

    if (binwidth <= 0 || is.na(binwidth)) {
      binwidth <- diff(range(x)) / 30
    }

    binwidth <- signif(binwidth, digits = decimal_precision(x) + 1)
  }

  return(binwidth)
}

compute_bins <- function(x, group = NULL, binwidth = NULL) {
  mask <- !is.na(x)
  x <- x[mask]
  group_vals <- if (is.null(group)) "All" else group[mask]

  if (length(x) == 0) return(NULL)

  if (is.null(binwidth)) binwidth <- compute_binwidth(x)

  bin_breaks <- seq(floor(min(x)), ceiling(max(x)) + binwidth, by = binwidth)
  bin_labels <- paste0("[", bin_breaks[-length(bin_breaks)], " – ", bin_breaks[-1], ")")

  bin_index <- cut(x, breaks = bin_breaks, labels = bin_labels, right = FALSE, include.lowest = TRUE)

  df_bins <- data.frame(
    x = x,
    fill = group_vals,
    bin = bin_index,
    stringsAsFactors = FALSE
  )

  df_bins <- df_bins %>%
    dplyr::group_by(fill, bin) %>%
    dplyr::summarise(count = length(x), .groups = "drop") %>%
    dplyr::mutate(
      xmin = as.numeric(sub(".*\\[| –.*", "", bin)),
      xmax = as.numeric(sub(".*– |\\)", "", bin)),
      text = paste0("Zakres: [", xmin, " – ", xmax, ")<br>Ilość: ", count)
    )

  return(df_bins)
}