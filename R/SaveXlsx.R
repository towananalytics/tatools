
#' save_xlsx
#'
#' @return Save dataframes as a Spreadsheet (.xlsx) to the home folder
#' @export
#' @import openxlsx
#' @import here
#'
#' @examples
#' 
#' a <- data.frame(mtcars)
#' b <- data.frame(letters)
#' 
#' data <- c("a", "b")
#' save_xlsx(data = c("a", "b"))


save_xlsx <- function(data = NULL,
                      file.name = "data export",
                      file.location = NA,
                      ...){

  wb <- openxlsx::createWorkbook()

  data.list <- lapply(data, get) # This creates a list of the objects.
  data.list <- setNames(data.list, data) # This names the elements of the list. 
  
  for(i in seq_along(data.list)){
    
    addWorksheet(wb, names(data.list)[i], gridLines = FALSE)
    
    openxlsx::writeData(wb = wb, 
                        x = data.list[i][[1]], 
                        startCol = 1, 
                        startRow = 1, 
                        borders = "rows", 
                        sheet = names(data.list)[i])
    
  }
  
  if(is.na(file.location)){
    
    file.location = here::here(paste0(Sys.Date(), " ", file.name, ".xlsx"))
    
  }
  
  saveWorkbook(wb, file.location, overwrite = TRUE, returnValue = FALSE)

}
