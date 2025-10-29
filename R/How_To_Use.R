#' Instrukcja obsługi pakietu
#'
#' Funkcja wypisuje na konsolę podstawowe informacje o pakiecie i kolejność użycia funkcji.
#'
#' @return Nic, wypisuje informacje na konsolę
#' @export
how_to_use <- function() {
  cat("Pakiet 'bkrzyEkspDane' – sugerowana kolejność użycia:\n\n")
  
  cat("1. data_report()      - aby zobaczyć wstępnie dane\n")
  cat("2. date_convert()     - konwersja kolumn z datami liczbowymi do formatu Date\n")
  cat("3. zero.handle()      - jeśli są kolumny, gdzie zera są błędami\n")
  cat("4. clip_range()       - przycinanie danych do min-max\n")
  cat("5. remove_outliers()  - przycinanie do 3σ lub 1.5 IQR\n")
  cat("6. num_to_qual()      - zamiana danych ilościowych na jakościowe\n")
  cat("7. date_visualization() lub date_visualization_simple() - wizualizacja danych\n")
  cat("8. plot_{x}()         - pojedynczy wykres w stylu wizualizacji\n\n")
  
  cat("Funkcje pomocnicze:\n")
  cat(" - get_color(), gg_color() - pomocnicze przy wizualizacji\n")
  cat(" - clear_all()             - czyszczenie środowiska\n")
  
  cat("\nUwaga: najlepiej postępować w kolejności podanej powyżej.\n")
}
