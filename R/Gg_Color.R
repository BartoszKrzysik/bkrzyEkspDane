#' Zwraca HEX kolorów z domyślnej palety ggplot2
#'
#' Funkcja przyjmuje nazwę koloru w formie tekstowej i zwraca odpowiadający mu
#' kolor HEX z domyślnej palety dyskretnych kolorów ggplot2 (`scale_color_hue`).
#' Obsługiwane kolory: `"red"`, `"green"`, `"blue"`, `"pink"`, `"yellow"`, `"cyan"`.
#'
#' @param color_name Nazwa koloru jako ciąg znaków. Obsługiwane nazwy: 
#'   `"red"`, `"green"`, `"blue"`, `"pink"`, `"yellow"`, `"cyan"`.
#' @return Ciąg znaków z kolorem w formacie HEX (np. `"#F8766D"`).
#' @examples
#' gg_color("red")
#' gg_color("green")
#' gg_color("blue")
#' @export
gg_color <- function(color_name) {
  switch(tolower(color_name),
         "red"   = "#F8766D",
         "green" = "#00BA38",
         "blue"  = "#619CFF",
         "pink"  = "#F564E3",
         "yellow"= "#B79F00",
         "cyan"  = "#00BFC4",
         stop("Nieznana nazwa koloru")
  )
}
