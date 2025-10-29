#' Przycinanie wartości do zadanego zakresu (dplyr)
#'
#' Funkcja zastępuje wszystkie wartości w wybranych kolumnach ramki danych, 
#' które są mniejsze od podanej wartości minimalnej lub większe od wartości maksymalnej, na `NA`. 
#' Można nadpisać oryginalne kolumny lub utworzyć nowe kolumny z oczyszczonymi wartościami.
#'
#' @param df Ramka danych zawierająca kolumny do przetworzenia.
#' @param cols Wektor znaków określający nazwy kolumn do oczyszczenia.
#' @param min_val Wartość minimalna – wszystkie wartości mniejsze zostaną zastąpione `NA`.
#' @param max_val Wartość maksymalna – wszystkie wartości większe zostaną zastąpione `NA`.
#' @param in_place Logiczne. Jeśli `TRUE`, nadpisuje oryginalne kolumny. 
#'        Jeśli `FALSE`, tworzy nowe kolumny z przyrostkiem "_clipped".
#'
#' @return Ramka danych z oczyszczonymi kolumnami.
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(a = c(1, 5, 10, 15, 20),
#'                  b = c(-5, 0, 5, 10, 15))
#' clip_range(df, c("a", "b"), min_val = 0, max_val = 15, in_place = TRUE)
#' clip_range(df, c("a", "b"), min_val = 0, max_val = 15, in_place = FALSE)
clip_range <- function(df, cols, min_val, max_val, in_place = TRUE) {
  if (!all(cols %in% names(df))) {
    stop("Niektóre kolumny nie istnieją w df")
  }
  
  if (in_place) {
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(cols),
          .fns = ~ ifelse(.x < min_val | .x > max_val, NA, .x)
        )
      )
  } else {
    new_cols <- paste0(cols, "_clipped")
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(cols),
          .fns = ~ ifelse(.x < min_val | .x > max_val, NA, .x),
          .names = "{.col}_clipped"
        )
      )
  }
  
  return(df)
}
