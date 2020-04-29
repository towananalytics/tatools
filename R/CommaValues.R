
#' Format large values to include comma separators and rounding
#'
#' @param x numerical value for which to add comma formatting to
#' @param scinot logical value to permit scientific notation in output value. Default is _FALSE_
#' @return A numerical value rounded to the nearest 2 decimal places
#' @examples
#' commas(12345610)
#' commas(12345610, TRUE)
#' commas(0.123456)
#' @export

commas <- function(x, scinot = FALSE) {
  if (scinot == FALSE)  options(scipen=999) else options(scipen=-1)
  return(format(x, digits = 2, big.mark = ","))
  options(scipen=1)
}
