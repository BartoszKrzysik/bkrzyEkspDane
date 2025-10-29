#' Zamienia kolumny numeryczne na jakościowe z automatycznymi etykietami lub własnymi labels
#'
#' Funkcja przekształca wskazane kolumny numeryczne (lub wszystkie numeryczne)
#' w kategorie jakościowe według określonych przedziałów.
#' Jeśli `labels` = NULL, etykiety są automatycznie generowane jako "A (low–high)".
#'
#' @param df Ramka danych.
#' @param cols Nazwy kolumn numerycznych do przekształcenia;
#'   jeśli NULL — wszystkie kolumny numeryczne.
#' @param breaks Liczba przedziałów lub wektor granic (jak w `cut()`).
#' @param labels (opcjonalnie) Własne etykiety dla przedziałów.
#' @param include.lowest logiczne; czy pierwszy przedział zawiera wartości minimalne (domyślnie TRUE).
#' @param prefix Prefiks nazw nowych kolumn (jeśli `replace = FALSE`).
#' @param replace logiczne; czy zastąpić oryginalne kolumny (TRUE) czy dodać nowe (FALSE).
#'
#' @return Ramka danych z kolumnami jakościowymi.
#' @export
#'
#' @examples
#' # automatyczne etykiety A (low–high)
#' num_to_qual(iris, cols = c("Sepal.Length", "Petal.Length"), breaks = 3)
#'
#' # własne etykiety
#' num_to_qual(iris, cols = "Sepal.Length", breaks = 3, labels = c("niski", "średni", "wysoki"))
#'
num_to_qual <- function(df,
                        cols = NULL,
                        breaks = NULL,
                        labels = NULL,
                        include.lowest = TRUE,
                        prefix = "cat_",
                        replace = TRUE) {
  fmt_num <- function(x) {
    s <- formatC(round(x, 3), format = "f", digits = 3)
    sub("\\.?0+$", "", s)
  }
  
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  
  if (length(cols) == 0)
    stop("Brak kolumn numerycznych do przekształcenia.")
  
  if (is.null(breaks))
    stop("Musisz podać liczbę przedziałów lub wektor granic (breaks).")
  
  df_new <- df
  
  for (col in cols) {
    x <- df[[col]]
    
    if (is.numeric(breaks) && length(breaks) == 1) {
      br_vec <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = breaks + 1)
    } else {
      br_vec <- sort(as.numeric(breaks))
    }
    
    n_intervals <- length(br_vec) - 1
    if (n_intervals < 1) stop("Niepoprawne granice (muszą tworzyć co najmniej 1 przedział).")
    
    if (!is.null(labels)) {
      if (length(labels) != n_intervals) {
        stop("Liczba etykiet (labels) musi odpowiadać liczbie przedziałów.")
      }
      final_labels <- labels
    } else {
      if (n_intervals <= length(LETTERS)) {
        letters_labels <- LETTERS[seq_len(n_intervals)]
      } else {
        warning("Liczba przedziałów > 26 — etykiety będą numerowane jako L1, L2, ...")
        letters_labels <- paste0("L", seq_len(n_intervals))
      }
      
      lows <- br_vec[seq_len(n_intervals)]
      highs <- br_vec[seq_len(n_intervals) + 1]
      range_str <- paste0("(", fmt_num(lows), "–", fmt_num(highs), ")")
      
      final_labels <- paste0(letters_labels, " ", range_str)
    }
    
    cat_var <- cut(
      x,
      breaks = br_vec,
      include.lowest = include.lowest,
      labels = final_labels
    )
    
    new_col <- if (replace) col else paste0(prefix, col)
    df_new[[new_col]] <- cat_var
    
    if (replace && new_col != col) {
      df_new[[col]] <- NULL
    }
  }
  
  df_new
}