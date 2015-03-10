#' Function getting historical Bloomberg data
#' @param tickers character vector specifying the securities
#' @param startdate numeric specified as "20000101" for "2000-01-01"
#' @export
#' @import Rbbg

HistEcoDates <- function(tickers = "EURR002W", startdate = "20000101") {
  
  conn <- blpConnect()
  
  bbg.data <- bdh(conn = conn,
                  securities = paste(tickers, "Index"),
                  fields = "ECO_RELEASE_DT",
                  start_date = startdate,
                  end_date = "")
 
  adj.data <- data.frame(report.date = bbg.data[, 1],
                         release.date = as.Date(paste(substr(bbg.data[,2], 1, 4),
                                                      substr(bbg.data[,2], 5, 6),
                                                      substr(bbg.data[,2], 7, 8),
                                                      sep = "-")))
  
  return(adj.data)
  
}