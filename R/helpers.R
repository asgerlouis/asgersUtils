#' Sorter kolonnenavne som er tal
#' funktionen kan tage en tabel og sortere kolonnenavne som er tal i rette order, selvom tallet jo er en tekststreng
#' @param data en tabel
#'
#' @return tabel med kolonner sorteret
#' @export
#'
sort_names <- function(data) {
  name  <- names(data)
  chars <- keep(name, grepl, pattern = "[^0-9]") %>% sort()
  nums  <- discard(name, grepl, pattern = "[^0-9]") %>% 
    as.numeric() %>% 
    sort() %>% 
    sprintf("%s", .)
  
  select(data, !!!c(chars, nums))
}