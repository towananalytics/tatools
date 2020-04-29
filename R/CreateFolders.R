
#' Create Project Directory
#'
#' @param extra Insert extra folders as required as a character vector
#' @import purrr
#' @return A project folder structure
#'
#' @export
#'
#' @examples
#' create_proj_dir()
#' create_proj_dir(c("Stuff", "More Stuff", "info", "info/dat"))
#'
create_proj_dir <- function(extra = NULL){

    folder_names <- c("analysis",           # Contains final RMarkdown Documents
                      "analysis/figures",   # Figures from RMDs (remeber to specify
                                            # output figure location)
                      "data",
                      "doc",    # Other documentation - non-R generated reports/
                                # proposals etc
                      "ext",    # External reports and info
                      "output", # Processed data (RDS files) and other processed tidy data
                      "output/models", # Processed data (RDS files) and models (RDS)
                      "prod",
                      "prod/shiny",    # Production Shiny apps or other app
                      "src")           # Source R code for functions ect to call into your RMD

    suppressWarnings(folder_names <- rbind(folder_names, extra))

    suppressWarnings(map(folder_names, dir.create))

}
