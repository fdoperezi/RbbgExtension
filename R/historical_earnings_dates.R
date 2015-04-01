#' Get historical earnings dates and figures across stocks
#' 
#' The function works for one or multiple tickers across the
#' equity class
#' 
#' @param tickers Character vector specifying the ticker code(s)
#' @details For a single ticker the function returns a data
#' frame containing historical earnings reporting period, 
#' release dates and earnings figures.
#' 
#' For multiple tickers the function returns a list of data
#' frames containing the same information.
#' 
#' The Announcement Time column is in local time.
#' 
#' The last row contains the next future earnings release 
#' including consensus EPS estimate for active tickers.
#' 
#' @export
#' @import Rbbg

HistEarnDates <- function(tickers = "JPM US") {
  
  tickers.type <- paste(tickers, "Equity", sep = " ")
  
  conn <- blpConnect()
  
  if(length(tickers) == 1) {
    
    bbg.data <- bds(conn = conn,
                    securities = tickers.type,
                    fields = "EARN_ANN_DT_TIME_HIST_WITH_EPS")
    
    output <- bbg.data[order(bbg.data[, "Announcement Date"]), ]
    
  } else {
    
    output <- vector("list", length(tickers))
      
    names(output) <- tickers
        
    for(i in 1:length(tickers)) {
      
      bbg.data <- bds(conn = conn,
                      securities = tickers.type[i],
                      fields = "EARN_ANN_DT_TIME_HIST_WITH_EPS")
      
      if(any(colnames(bbg.data) == "Announcement Date")) {
        
        output[[i]] <- bbg.data[order(bbg.data[, "Announcement Date"]), ]
        
      }
      
    }
    
  }
  
  return(output)
  
}