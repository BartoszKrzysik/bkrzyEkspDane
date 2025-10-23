#' Przygotowuje ramkę danych do wizualizacji
#'
#' Funkcja dodaje kolumnę "All" jeśli nie podano kolumn jakościowych,
#' lub konwertuje istniejące kolumny jakościowe na faktory lokalnie.
#'
#' @param df Ramka danych
#' @param qualitative_cols Wektor nazw kolumn jakościowych (faktor/character) lub NULL
#' @return Lista z elementami:
#'   \item{df}{zmodyfikowana lokalnie ramka danych}
#'   \item{qualitative_cols}{przetworzony wektor kolumn jakościowych}
prepare_qualitative <- function(df, qualitative_cols = NULL) {
  df_local <- df
  
  if (is.null(qualitative_cols) || length(qualitative_cols) == 0) {
    df_local$All <- "All"
    qualitative_cols <- "All"
  } else {
    for (col in qualitative_cols) {
      if (col %in% names(df_local) && !is.factor(df_local[[col]])) {
        df_local[[col]] <- as.factor(df_local[[col]])
      }
    }
  }
  
  list(df = df_local, qualitative_cols = qualitative_cols)
}
