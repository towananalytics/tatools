#' Insert the cardinal compass direction for given decmimal degress
#'
#' This function inserts a new variable "cardinal.dir" into the dataframe passed to it..
#'
#' @param data a dataframe containing the data
#' @param !!dir_col direction column to convert to cardinal factor.
#' @return the original dataframe with the cardinal varaible included
#' @import dplyr
#' @examples
#'
#' insert_cardinal_dir(waves, dir_deg)
#'
#' #use with dply
#' library(dplyr)
#' waves %>% insert_cardinal_dir(waves, dir_deg)


insert_cardinal_dir <- function(data, dir_col) {
    # Check that data is in decimal degress:
   #if (data[, deparse(substitute(dir_col))] > 360) stop("Directions are not in degrees", FALSE)

    dat <- data
    dir_col <- enquo(dir_col)
    deparse(substitute(dir_col)) # for labelling
    # look up this tutorial to insert data from dplyr:
    # https://medium.com/optima-blog/writing-your-own-dplyr-functions-a1568720db0d
# dat %>% select(.dots = lazyeval::lazy_dots(!!dir_col)) %>%
    dat <- dat %>%
           mutate(cardinal.dir = factor(ifelse(!!dir_col >= 0 & !!dir_col <= 22.5,
                                          "N",
                                          ifelse(!!dir_col > 22.5 & !!dir_col <= 67.5,
                                                 "NE",
                                                 ifelse(!!dir_col > 67.5 & !!dir_col <= 112.5,
                                                        "E",
                                                        ifelse(!!dir_col > 112.5 & !!dir_col <= 157.5,
                                                               "SE",
                                                               ifelse(!!dir_col > 157.5 & !!dir_col <= 202.5,
                                                                      "S",
                                                                      ifelse(!!dir_col > 202.5 & !!dir_col <= 247.5,
                                                                             "SW",
                                                                             ifelse(!!dir_col > 247.5 & !!dir_col <= 292.5,
                                                                                    "W",
                                                                                    ifelse(!!dir_col > 292.5 & !!dir_col <= 337.5,
                                                                                           "NW",
                                                                                           ifelse(!!dir_col > 337.5 & !!dir_col <= 360,
                                                                                                  "N", ""
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
           )
           ))

return(dat)


}