#' Konwertuje kolumny z datami na obiekt klasy Date
#'
#' Funkcja automatycznie konwertuje wskazane kolumny w ramce danych na typ \code{Date}.
#' Obsługiwane są różne formaty dat, m.in. rok-miesiąc-dzień, dzień.miesiąc.rok itp.
#' Jeśli kolumna zawiera tylko rok i miesiąc, dodawany jest pierwszy dzień miesiąca.
#'
#' @param df Ramka danych zawierająca kolumny do konwersji.
#' @param cols Wektor nazw kolumn, które mają zostać skonwertowane.
#'
#' @return Ramka danych z kolumnami zamienionymi na typ \code{Date}.
#' @examples
#' df <- data.frame(date1 = c("2023.06", "2023.07"), date2 = c("01-06-2023", "15-07-2023"))
#' date_convert(df, c("date1", "date2"))
#' @export
date_convert <- function(df, cols) {
  
  possible_formats <- c(
    "Y-m-d", "Y/%m/%d", "Y.%m.%d",
    "Y-m", "Y/%m", "Y.%m",      
    "d.m.Y", "d/m/Y", "d-%m-%Y" 
  )
  df_new <- df
  for (col in cols) {
    if (!col %in% names(df_new)) {
      warning(paste("Kolumna", col, "nie istnieje w ramce danych. Pomijam."))
      next
    }
    parsed <- parse_date_time(df_new[[col]], orders = possible_formats, quiet = TRUE)
    
    if (any(is.na(parsed)) && grepl("^\\d{4}[./-]\\d{2}$", df_new[[col]][1])) {
      parsed <- parse_date_time(paste0(df[[col]], ".01"), orders = c("Y.m.d", "Y-%m-%d", "Y/%m/%d"), quiet = TRUE)
    }
    
    df_new[[col]] <- as.Date(parsed)
  }
  
  return(df_new)
}
