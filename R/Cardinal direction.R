#' Insert the cardinal compass direction for given decimal degrees
#'
#' This function inserts a new variable "cardinal.dir" into the dataframe passed to it..
#'
#' @param data a dataframe containing the data.
#' @param dir_col direction column to convert to cardinal factor.
#' @param abbr TRUE/FALSE should the results be abbreviated (N versus North)
#' @return the original dataframe with the cardinal variable included
#' @import dplyr
#' @examples
#'
#' insert_cardinal_dir(waves, dir_deg)
#'
#' #use with dplyr
#' library(dplyr)
#' waves %>% insert_cardinal_dir(waves, dir_deg)

#' @export

insert_cardinal_dir <- function(data, dir_col, abbr = TRUE) {
    # Check that data is in decimal degrees:
   #if (data[, deparse(substitute(dir_col))] > 360) stop("Directions are not in degrees", FALSE)

    dat <- data
    dir_col <- enquo(dir_col)
    deparse(substitute(dir_col)) # for labeling

    if(abbr == TRUE) { # Abbreviated response

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

    } else { # Non-abbreviated response

        dat <- dat %>%
            mutate(cardinal.dir = factor(ifelse(!!dir_col >= 0 & !!dir_col <= 22.5,
                                                "North",
                                                ifelse(!!dir_col > 22.5 & !!dir_col <= 67.5,
                                                       "North East",
                                                       ifelse(!!dir_col > 67.5 & !!dir_col <= 112.5,
                                                              "East",
                                                              ifelse(!!dir_col > 112.5 & !!dir_col <= 157.5,
                                                                     "South East",
                                                                     ifelse(!!dir_col > 157.5 & !!dir_col <= 202.5,
                                                                            "South",
                                                                            ifelse(!!dir_col > 202.5 & !!dir_col <= 247.5,
                                                                                   "South West",
                                                                                   ifelse(!!dir_col > 247.5 & !!dir_col <= 292.5,
                                                                                          "West",
                                                                                          ifelse(!!dir_col > 292.5 & !!dir_col <= 337.5,
                                                                                                 "North West",
                                                                                                 ifelse(!!dir_col > 337.5 & !!dir_col <= 360,
                                                                                                        "North", ""
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

    }

return(dat)

}
