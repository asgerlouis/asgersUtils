
#' lav en tabel der viser antal for hver string_length pr. kolonne
#'
#' @param tabel en tabel med kolonner
#'
#' @return en tabel med kolonner og antal for hver string-length
#' @import dplyr
#' @import purrr
#' @importFrom magrittr "%>%"
#' @export
#'
kolonner_string_length <- function(tabel) {
  # fjerner kolonner hvor der kun er NAer og giver warning
  mulige_tomme_kolonner <- tabel %>% 
    purrr::keep(~all(is.na(.x))) %>% 
    names
  if(length(mulige_tomme_kolonner > 0)) {warning(paste(mulige_tomme_kolonner, "har kun NA-values"))}
  
  kolonne_navne <-  tabel %>% 
    purrr::discard(~all(is.na(.x))) %>% 
    names()

  output <- tabel %>% 
    purrr::discard(~all(is.na(.x))) %>%
    purrr::map(~table(stringr::str_length(.x))) %>% 
    purrr::map_df(bind_rows) %>% 
    dplyr::bind_cols(data.frame("col_name" = kolonne_navne)) %>% 
    asgersUtils::sort_names()
}