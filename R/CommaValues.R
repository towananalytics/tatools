
#' Format large values to include comma separators and rounding
#'
#' @param x numerical value for which to add comma formatting to
#' @param scinot logical value to permit scientific notation in output value. Default is _FALSE_
#' @return A numerical value rounded to the nearest 2 decimal places
#' @examples
#' commas(12345610)
#' commas(12345610, TRUE)
#' commas(12345610.09324500, TRUE, digits = 2)
#' commas(0.123456)
#' @export

commas <- function(x, scinot = FALSE, digits = 2) {

  if(digits < 0){
    stop(paste0("digit arg `", digits, "` must be >= 0"))
  }

  scipen_opt <- getOption("scipen") # The initial scipen option - incase the user has set it

  if (scinot == FALSE) options(scipen = 999) else options(scipen = -1)

  # val <- format(x, digits = digits, big.mark = ",")

  val <- format(round(as.numeric(x), digits), nsmall=digits, big.mark=",")

  options(scipen = scipen_opt)

  return(val)

  }
