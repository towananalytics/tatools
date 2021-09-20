
#' save_xlsx
#'
#' @return Saved data frames as .xlsx format to the home folder
#' @param data Vector of strings containing the name of the data frame objects
#' @param file.name the name of the output excel file, excluding the extension
#' @param file.location the path to the save the file, by default, this is the project home folder
#' @param apply.header Default is FALSE. If enabled, and without header.title arguments, the dataframe object names are inserted in the header title
#' @param header.title a string vector to override the default data frame object names in the header output. The length of this vector must equal the number of data frames
#' @param apply.style logical argument to include a standard styling to the output. Styling includes header and body styles
#' @param start.rows Vector of integers specifying the row index to insert the data. Vector length should equal the number of data frame objects
#' @param start.cols Vector of integers specifying the column index to insert the data. Vector length should equal the number of data frame objects
#' @export
#' @import openxlsx
#' @import here
#'
#' @examples
#' 
#' a <- data.frame(mtcars)
#' b <- data.frame(letters)
#' sleep <- data.frame(msleep)
#' 
#' save_xlsx(data = c("a", "b"))
#' save_xlsx(data = c("a", "b"), apply.style = TRUE)
#' save_xlsx(data = c("a", "b"), apply.style = TRUE, start.rows = c(1, 2))
#' save_xlsx(data = c("a", "b"), apply.style = TRUE, start.rows = c(1, 2), start.cols = c(3, 4))
#' save_xlsx(data = c("a", "b", "sleep"), apply.style = TRUE, start.rows = c(1, 2, 2), start.cols = c(3, 4, 2))
#' save_xlsx(data = c("a", "b", "sleep"), apply.style = TRUE, file.name = "new file")
#' save_xlsx(data = c("a", "b", "sleep"), apply.header = TRUE)
#' save_xlsx(data = c("a", "b", "sleep"), apply.header = TRUE, file.location = getwd())
#' save_xlsx(data = c("a", "b", "sleep"), header.title = c("sheet1 header", "sheeet2 header", "sheet3 header"))



save_xlsx <- function(data = NULL,
                      file.name = NA,
                      file.location = NA,
                      apply.header = FALSE,
                      header.title = NA,
                      apply.style = FALSE,
                      start.rows = NA,
                      start.cols = NA,
                      ...){
  
  colour_pallette <- c("#5F6062", "#008C98", "#982623", "#B2B2B1", "#C49E39",
                       "#295775", "#175E62", "#8E9CA3", "#556670", "#000000")
  
  headerStyle <- openxlsx::createStyle(
    fontSize = 11, fontColour = "#FFFFFF", halign = "center", valign = "center",
    fgFill = colour_pallette[2], border = "TopBottom", borderColour = "#4F81BD", wrapText = TRUE
    )
  
  ## style for body
  bodyStyle <- openxlsx::createStyle(
    border = "TopBottom", borderColour = "#4F81BD", valign = "center", halign = "center", wrapText = TRUE
    )

  wb <- openxlsx::createWorkbook()

  data.list <- lapply(data, get) # This creates a list of the objects.
  data.list <- setNames(data.list, data) # This names the elements of the list. 

# Assign start rows and columns if a vector was not provided --------------
  
  if(all(!is.na(start.rows)==TRUE)){
    # check if length of entries matches length of data
    if((length(start.rows) != length(data.list)) == TRUE){
      start.rows <- rep(1, length(data.list))
      message("Vector length of start rows is less than number of data objects - Assigning a value of 1 instead")
    }
  }
  
  if(all(!is.na(start.cols)==TRUE)){
    # check if length of entries matches length of data
    if((length(start.cols) != length(data.list)) == TRUE){
      start.cols <- rep(1, length(data.list))
      message("Vector length of start columns is less than number of data objects - Assigning a value of 1 instead")
    }
  }
  
  if(any(is.na(start.rows))==TRUE){
    start.rows <- rep(1, length(data.list))
  }
  
  if(any(is.na(start.cols))==TRUE){
    start.cols <- rep(1, length(data.list))
  }
  
  for(i in seq_along(data.list)){
    
    sheet.name <- names(data.list)[i]
    
    addWorksheet(wb, sheet.name, gridLines = FALSE)
    
    openxlsx::writeData(wb = wb, 
                        x = data.list[i][[1]], 
                        startCol = start.cols[i], 
                        startRow = start.rows[i], 
                        borders = "rows", 
                        sheet = sheet.name)
    
    if(apply.style == TRUE){
      
      openxlsx::addStyle(style = headerStyle, 
                         wb = wb, 
                         sheet = sheet.name, 
                         rows = start.rows[i], 
                         cols = (start.cols[i]:(length(data.list[i][[1]]) + (start.cols[i] - 1))), 
                         gridExpand = FALSE, 
                         stack = TRUE)
      
      openxlsx::addStyle(style = bodyStyle, 
                         wb = wb, 
                         sheet = sheet.name,
                         rows = c((start.rows[i] + 1):(start.rows[i] + nrow(data.list[i][[1]]))), 
                         cols = c(start.cols[i]:(length(data.list[i][[1]]) + (start.cols[i] - 1))), 
                         gridExpand = TRUE)
    
    }
    
    if(apply.header == TRUE | all(!is.na(header.title) == TRUE)){
    
      if(any(is.na(header.title))){
        
        header.title <- names(data.list)
        
      }
      
      setHeaderFooter(wb, sheet = sheet.name, 
                      header = c(paste0('&"Arial"&B&14&K008C98', header.title[i]), 
                                 NA, 
                                 NA),
                      footer = c("Printed On: &[Date]", NA, "Page &[Page] of &[Pages]"),)
    
    }
    
  }
  
  if(is.na(file.name)){
    
    file.name <- paste0(paste0(Sys.Date(), " ", "Data Output"))
    
  }
  
  if(is.na(file.location)){
    
    file.location <- here::here()
    
  }
  
  file.name <- paste0(file.name, ".xlsx")
  
  saveWorkbook(wb, paste0(file.location, "/", file.name), overwrite = TRUE, returnValue = FALSE)

}
