#' Zwraca kolor z podstawowej palety lub losowy kolor
#'
#' Funkcja zwraca kolor w postaci nazwy lub kodu HEX. 
#' Jeśli podana liczba `n` mieści się w zakresie podstawowej palety,
#' zwracany jest kolor z tej palety. Jeśli `n` jest większe od długości
#' palety, generowany jest losowy kolor w formacie HEX.
#'
#' @param n Liczba całkowita >= 1 określająca indeks koloru.
#' @return String z nazwą koloru (np. "red") lub kodem HEX (np. "#A1B2C3").
#' @examples
#' get_color(3)    # zwraca "green"
#' get_color(15)   # zwraca losowy kolor HEX
#'
#' @export
get_color <- function(n) {
  basic_colors <- c("red", "blue", "green", "yellow", "orange", "purple", 
                    "pink", "brown", "cyan", "magenta", "gray", "black")
  
  if (n >= 1 && n <= length(basic_colors)) {
    return(basic_colors[n])
  } else if (n > length(basic_colors)) {
    return(sprintf("#%06X", sample(0:0xFFFFFF, 1)))
  } else {
    stop("Argument musi być liczbą całkowitą >= 1")
  }
}
