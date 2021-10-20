
#' save_xlsx
#' @description Wrapper for openxlsx to easily output a data frame as an excel spreadsheet.
#' @return Saved data frames as .xlsx format to the home folder
#' @param data vector of strings containing the name(s) of data frame objects
#' @param file.name the file name of the excel file, \emph{excluding} the path and extension details
#' @param file.location the file path to save the file, by default, this is the project home folder
#' @param apply.header default is \code{FALSE}. If enabled, and without \code{header.title} arguments, the data frame object names are as the header titles
#' @param header.title a string vector to override the default data frame object names in the header output. The length of this vector must equal the number of data frames
#' @param header.font.style string vector options are limited to standard font styles
#' @param header.font.size integer value
#' @param header.font.colour hex colour format without the hash (#) prefix
#' @param apply.style logical argument to include a standard styling to the output. Styling includes header and body styles
#' @param borders string vector that applies a border style to the output data. Default is "rows". Options are "none", "surrounding", "rows", "columns", "all". Vector length should equal the number of data frame objects
#' @param start.rows Vector of integers specifying the row index to insert the data. Vector length should equal the number of data frame objects
#' @param start.cols Vector of integers specifying the column index to insert the data. Vector length should equal the number of data frame objects
#' @param body.wrap.text wrap body text in table outputs. Logical argument with a default value of \code{FALSE} 
#' @param body.valign vertical alignment of body text. Default is \code{"center"}, options are \code{"center", "top", "bottom"}. Can be a vector of length 1 or separate arguments for each data frame
#' @param body.halign horizontal alignment of body text. Default is \code{"left"}, options are \code{"left", "center", "right"}. Can be a vector of length 1 or separate arguments for each data frame
#' @param withFilter vector of logical values to include filters at the column names in the spreadsheet. Default is FALSE. Vector length should equal the number of data frame objects.
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
#' save_xlsx(data = c("a", "b", "sleep"), header.title = c("sheet1 header", "sheet2 header", "sheet3 header"))
#' save_xlsx(data = c("a", "b"), apply.style = TRUE, body.halign = "center")
#' save_xlsx(data = c("a", "b"), apply.style = TRUE, body.halign = c("center", "right"))

save_xlsx <- function(data = NULL,
                      file.name = NA,
                      file.location = NA,
                      apply.header = FALSE,
                      header.title = NA,
                      header.font.style = "Arial",
                      header.font.size = 12,
                      header.font.colour = "008C98",
                      apply.style = FALSE,
                      borders = NA, 
                      start.rows = NA,
                      start.cols = NA,
                      body.wrap.text = FALSE,
                      body.valign = "center",
                      body.halign = "left",
                      withFilter = NA
                      ){
  
  wb <- openxlsx::createWorkbook()
  
  data.list <- lapply(data, get) # This creates a list of the objects.
  data.list <- setNames(data.list, data) # This names the elements of the list
  
  colour_pallette <- c("#5F6062", "#008C98", "#982623", "#B2B2B1", "#C49E39",
                       "#295775", "#175E62", "#8E9CA3", "#556670", "#000000")
  
  if((length(body.halign) != length(data.list)) == TRUE){
    #If the length of the vector does not equal the data frame length then replicate the argument
    #In case the input vector length is greater than the number of data frames, the first in the sequence is replicated 
    body.halign <- rep(body.halign[1], length(data.list))
  }
  
  if((length(body.valign) != length(data.list)) == TRUE){
    #If the length of the vector does not equal data frame length then replicate the argument
    #In case the input vector length is greater than the number of data frames, the first in the sequence is replicated 
    body.valign <- rep(body.valign[1], length(data.list))
  }
  
  if((length(body.wrap.text) != length(data.list)) == TRUE){
    #If the length of the vector does not equal data frame length then replicate the argument
    #In case the input vector length is greater than the number of data frames, the first in the sequence is replicated 
    body.wrap.text <- rep(body.wrap.text[1], length(data.list))
  }
  
  headerStyle <- openxlsx::createStyle(
    fontSize = 11, fontColour = "#FFFFFF", halign = "center", valign = "center",
    fgFill = colour_pallette[2], border = "TopBottom", borderColour = "#4F81BD", wrapText = TRUE
  )

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
  
  if(any(is.na(borders))==TRUE){
    borders = rep("rows", length(data.list))
  }
  
  if(any(is.na(withFilter))==TRUE){
    withFilter = rep(FALSE, length(data.list))
  }
  
  for(i in seq_along(data.list)){
    
    sheet.name <- names(data.list)[i]
    
    openxlsx::addWorksheet(wb, sheet.name, gridLines = FALSE)
    
    openxlsx::writeData(wb = wb, 
                        x = data.list[i][[1]], 
                        startCol = start.cols[i], 
                        startRow = start.rows[i], 
                        borders = borders[i],
                        sheet = sheet.name, 
                        withFilter = withFilter[i])
    
    if(apply.style == TRUE){
      
      openxlsx::addStyle(style = headerStyle, 
                         wb = wb, 
                         sheet = sheet.name, 
                         rows = start.rows[i], 
                         cols = (start.cols[i]:(length(data.list[i][[1]]) + (start.cols[i] - 1))), 
                         gridExpand = FALSE, 
                         stack = TRUE)
      
      ## style for body
      bodyStyle <- openxlsx::createStyle(
        border = "TopBottom", 
        borderColour = "#4F81BD", 
        valign = body.valign[i], 
        halign = body.halign[i], 
        wrapText = body.wrap.text[i]
      )
      
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
      
      setHeaderFooter(wb, 
                      sheet = sheet.name, 
                      header = c(paste0(paste0('&"', header.font.style, '"&', 'B','&', header.text.size, '&K', header.text.colour), header.title[i]), 
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
