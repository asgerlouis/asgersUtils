#' Antal na pr. kolonner
#' Funktionen tager en tabel og tæller NA'er i hver kolonne
#'
#' @param tabel bare en tabel
#'
#' @return en navngivet numerisk vector
#' @export
#' @examples
#' x <- data.frame("tal"=c(1,2,3), "bogstaver"= c(NA, "b", "c"))
#' kolonner_med_na(x)
kolonner_med_na <- function(tabel) {
  apply(tabel,2, function(x) sum(is.na(x)))
}
