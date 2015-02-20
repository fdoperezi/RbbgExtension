#' Get historical earnings dates across stocks
#' 
#' The function works for one or multiple tickers across the
#' equity class
#' 
#' @param tickers Character vector specifying the ticker code(s)
#' @details For a single ticker the function returns a data
#' frame containing historical earnings release dates and the
#' reporting period.
#' 
#' For multiple tickers the function returns a list of data
#' frames containing the same information.
#' 
#' @export
#' @import Rbbg
#' @import stringr

HistEarnDates <- function(tickers = "JPM US") {
  
  tickers.type <- paste(tickers, "Equity", sep = " ")
  
  conn <- blpConnect()
  
  bbg.data <- bds(conn = conn,
                  securities = tickers.type,
                  fields = "ERN_ANN_DT_AND_PER")
  
  if(length(tickers) == 1) {
    
    primary.period <- bdp(conn = conn,
                          securities = tickers.type,
                          fields = "PRIMARY_PERIODICITY")
    
    primary.period <- ifelse(str_detect(primary.period[, 1],
                                        "\\bQuarterly\\b"), "Q",
                      ifelse(str_detect(primary.period[, 1],
                                        "\\bSemi-Annual\\b"), "S", NA))
    
    output <- bbg.data[order(bbg.data[, 1]), ]
    
    period.match <- str_detect(output[, 2], primary.period)
   
    output <- output[period.match, ]
    
  }
  
  if(length(tickers) > 1) {
    
    output <- vector("list", length(tickers))
    
    names(output) <- tickers
    
    primary.period <- bdp(conn = conn,
                          securities = tickers.type,
                          fields = "PRIMARY_PERIODICITY")
    
    primary.period <- ifelse(str_detect(primary.period[, 1],
                                        "\\bQuarterly\\b"), "Q",
                             ifelse(str_detect(primary.period[, 1],
                                               "\\bSemi-Annual\\b"), "S", NA))
    
    ticker.levels <- levels(bbg.data[, "ticker"])
    
    for(i in 1:length(tickers)) {
      
      ticker.pos <- which(bbg.data[, "ticker"] == ticker.levels[i])
      
      adj.data <- bbg.data[ticker.pos, ]
      
      adj.data <- adj.data[order(adj.data[, 2]), ]
      
      period.match <- str_detect(adj.data[, 3], primary.period[i])
      
      output[[i]] <- data.frame(adj.data[period.match, -1],
                                row.names = NULL)
      
    }
    
  }

  return(output)
  
}