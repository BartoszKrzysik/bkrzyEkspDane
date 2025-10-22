#' Wyczyść środowisko i konsolę
#'
#' Funkcja usuwa wszystkie obiekty z globalnego środowiska,
#' odładowuje wszystkie niebazowe pakiety, czyści konsolę
#' oraz uruchamia garbage collector, aby zwolnić pamięć.
#'
#' @return NULL. Funkcja działa poprzez efekt uboczny na środowisko.
#' @examples
#' clear_all()
#'
#' @export
clear_all <- function() {
  # Wyczyść konsolę (działa w RStudio i R GUI)
  cat("\014")
  
  # Usuń wszystkie obiekty z globalnego środowiska
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Odładuj wszystkie załadowane pakiety poza bazowymi
  base_pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")
  loaded_pkgs <- loadedNamespaces()
  to_detach <- setdiff(loaded_pkgs, base_pkgs)
  
  for (pkg in to_detach) {
    try({
      unloadNamespace(pkg)
    }, silent = TRUE)
  }
  
  # Wyczyść pamięć
  invisible(gc())
}
