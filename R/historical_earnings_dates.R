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
#' including consensus EPS estimate.
#' 
#' @export
#' @import Rbbg

HistEarnDates <- function(tickers = "JPM US") {
  
  tickers.type <- paste(tickers, "Equity", sep = " ")
  
  conn <- blpConnect()
  
  bbg.data <- bds(conn = conn,
                  securities = tickers.type,
                  fields = "EARN_ANN_DT_TIME_HIST_WITH_EPS")
  
  if(length(tickers) > 1) {
    
    output <- vector("list", length(tickers))
    
    names(output) <- tickers
    
    ticker.levels <- levels(bbg.data[, "ticker"])
    
    for(i in 1:length(tickers)) {
      
      ticker.pos <- which(bbg.data[, "ticker"] == ticker.levels[i])
      
      adj.data <- bbg.data[ticker.pos, ]
      
      output[[i]] <- data.frame(adj.data[order(adj.data[, "Announcement.Date"]), -1],
                                row.names = NULL)
      
    }
    
  } else {
    
    output <- bbg.data[order(bbg.data[, "Announcement Date"]), ]
    
  }

  return(output)
  
}