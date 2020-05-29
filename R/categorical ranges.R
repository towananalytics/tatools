#' Insert the categorical label for a vector of numerical values.
#'
#' Numerical data is passed along with the nunber of bins required. A label of binned factor ranges is returned
#'
#' @param data a dataframe containing the data
#' @param num_col is the column name containing the merical values to be binned and labelled.
#' @param bins is the number of bins to assign the categorical labels 
#' @return the categorical labels as a factor
#' @import dplyr
#' @examples
#' 
#' # Create some data
#' samples <- data.frame(samp = sample(1:1000, 500, replace = T))
#'
#' cat_num(samples, samp)
#' 
#' # Use with dplyr
#' 
#' library(dplyr)
#' samples %>% cat_num(samp)

#' @export


cat_num <- function(data, num_col, bins = 4){
    
    if(bins < 2) stop("value for bins cannot be less than 2", call. = F)
    
    num_col <- enquo(num_col)
    
    dat <- data %>% mutate(label = cut(!!num_col, breaks = bins),
                           label = factor(paste0(round(as.numeric( sub("\\((.+),.*", "\\1", label) ), 2), 
                                                    "-", 
                                                    round(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", label) ), 2)))
                           
    ) %>% 
        select(label)
    
     return(dat)
    
}


