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
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)

  if (is.null(binwidth)) binwidth <- compute_binwidth(x)

  bin_breaks <- seq(floor(min(x)), ceiling(max(x)) + binwidth, by = binwidth)
  xmin_vec <- bin_breaks[-length(bin_breaks)]
  xmax_vec <- bin_breaks[-1]
  bin_labels <- paste0("[", xmin_vec, " – ", xmax_vec, ")")
  bin_index <- cut(x, breaks = bin_breaks, labels = seq_along(xmin_vec), right = FALSE, include.lowest = TRUE)
  bin_index_num <- as.integer(bin_index)

  df_bins <- data.frame(
    x = x,
    fill = if (is.null(group)) "All" else group[!is.na(x)],
    bin = bin_labels[bin_index_num],
    xmin = xmin_vec[bin_index_num],
    xmax = xmax_vec[bin_index_num]
  )

  df_bins <- df_bins %>%
    dplyr::group_by(fill, bin, xmin, xmax) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::mutate(
      text = paste0("Zakres: [", xmin, " – ", xmax, ")<br>Ilość: ", count)
    )

  return(df_bins)
}
