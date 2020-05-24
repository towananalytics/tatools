#' Insert the season into the dataframe passed to the function
#'
#' This function produces a season variable stored as a factor.
#'
#' @param data a dataframe containing the data
#' @param date_col Date variable containing the date to be used in calculating season.
#' @param hemis Hemishere for calculating season "north" or "south"
#' @importFrom lubridate month
#' @return the original dataframe with the season varaible included
#' @examples
#'
#' insert_season(waves, date_time)
#'
#' #using dplyr
#' library(dplyr)
#' waves %>% insert_season(date_time)

insert_season <- function(data, date_col, hemis = north){

    dat <- as.data.frame(data) # Ensure data is in df format
   # date_col <- as.Date(dat$date_col)

    date_col <- enquo(date_col)
    deparse(substitute(date_col)) # for labelling

    if(tolower(deparse(substitute(hemis))) == "north") { # Northern hemisphere

        dat <- dat %>% mutate(Season = factor(case_when(month(!!date_col) == 1 ~ "Summer",
                                              month(!!date_col) ==2 ~ "Summer",
                                              month(!!date_col) == 3 ~ "Autumn",
                                              month(!!date_col) == 4 ~ "Autumn",
                                              month(!!date_col) == 5 ~ "Autumn",
                                              month(!!date_col) == 6 ~ "Winter",
                                              month(!!date_col) == 7 ~ "Winter",
                                              month(!!date_col) == 8 ~ "Winter",
                                              month(!!date_col) == 9 ~ "Spring",
                                              month(!!date_col) == 10~ "Spring",
                                              month(!!date_col) == 11 ~ "Spring",
                                              month(!!date_col) == 12 ~ "Summer",
                                              TRUE ~ as.character(month(!!date_col))
                                              )
                                              )
                              )
        return(dat)

    } else { # Southern hemisphere

        dat <- dat %>% mutate(Season = factor(case_when(month(!!date_col) == 1 ~ "Winter",
                                                        month(!!date_col) == 2 ~ "Winter",
                                                        month(!!date_col) == 3 ~ "Spring",
                                                        month(!!date_col) == 4 ~ "Spring",
                                                        month(!!date_col) == 5 ~ "Spring",
                                                        month(!!date_col) == 6 ~ "Summer",
                                                        month(!!date_col) == 7 ~ "Summer",
                                                        month(!!date_col) == 8 ~ "Summer",
                                                        month(!!date_col) == 9 ~ "Autumn",
                                                        month(!!date_col) == 10~ "Autumn",
                                                        month(!!date_col) == 11 ~ "Autumn",
                                                        month(!!date_col) == 12 ~ "Winter",
                                                        TRUE ~ as.character(month(!!date_col))
                                                        )
                                              )
                              )
        return(dat)

    }


}
