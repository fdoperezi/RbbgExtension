#' Function getting historical Bloomberg data
#' @param tickers character vector specifying the macro index
#' @param startdate numeric specifying date as "20000101" for "2000-01-01"
#' @param append.values boolean specifying whether macro data should be
#' appended to output
#' @details The reason why the append.values option exist is that release
#' figures does not always make sense for a macro series such as "EURR002W Index"
#' related to ECB Main Refinancing Operations Announcement Rate
#' @export
#' @import Rbbg

HistEcoDates <- function(tickers = "EURR002W", startdate = "20000101",
                         append.values = FALSE) {
  
  conn <- blpConnect()
  
  if(append.values) {
    
    fields <- c("ECO_RELEASE_DT", "PX_LAST", "ACTUAL_RELEASE", "FIRST_REVISION")
    
    columns <- c("report.date", "release.date", "final", "actual",
                 "first.revision")
    
  } else {
    
    fields <- "ECO_RELEASE_DT"
    
    columns <- c("report.date", "release.date")
    
  }
  
  bbg.data <- bdh(conn = conn,
                  securities = paste(tickers, "Index"),
                  fields = fields,
                  start_date = startdate,
                  end_date = "")
  
  bbg.data[, 2] <- ifelse(is.na(bbg.data[, 2]), NA,
                          paste(substr(bbg.data[,2], 1, 4),
                                substr(bbg.data[,2], 5, 6),
                                substr(bbg.data[,2], 7, 8),
                                sep = "-"))
  
  adj.data <- data.frame(bbg.data,
                         stringsAsFactors = FALSE)
  
  colnames(adj.data) <- columns
  
  return(adj.data)
  
}