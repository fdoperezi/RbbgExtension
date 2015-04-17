
#' Function that returns market data (Open, High, Low, Close, Volume)
#' @param tickers Character vector of ticker names (see details)
#' @param type Character that specifying instrument type (see details)
#' @details The function returns conventional market data such as open, 
#' high, low, close, volume. The output object is either a xts 
#' object or array depending on the number of tickers provided (see details).
#' 
#' Tickers are structured as symbols together with the exchange code e.g.
#' AAPL US for Apple across aggregate US exchanges (composite code) and
#' AAPL UW for Apple's price data from NASDAQ Global Select
#' 
#' @export
#' @import Rbbg
#' @import xts
#' @examples
#' MarketData(index = "AAPL US", startdate = "20140101", enddate = "20140630")

MarketData <- function(tickers = "AAPL US",
                       type = "Equity",
                       freq = "DAILY",
                       currency = NULL,
                       startdate = "20140101",
                       enddate = "",
                       non.trading.days = "NON_TRADING_WEEKDAYS",
                       non.trading.days.fill = "NIL_VALUE") {
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  if(is.null(currency)) {
    
    option.names  <- c("periodicitySelection",
                       "nonTradingDayFillOption",
                       "nonTradingDayFillMethod")
    
    option.values <- c(freq,
                       non.trading.days,
                       non.trading.days.fill)
    
  } else {
    
    option.names  <- c("periodicitySelection",
                       "currency",
                       "nonTradingDayFillOption",
                       "nonTradingDayFillMethod")
    
    option.values <- c(freq,
                       currency,
                       non.trading.days,
                       non.trading.days.fill)
    
  }
  
  conn <- blpConnect()
  
  bbg.data <- bdh(conn = conn,
                  securities = tickers.type,
                  fields = c("PX_OPEN",
                             "PX_HIGH",
                             "PX_LOW",
                             "PX_LAST",
                             "PX_VOLUME"),
                  start_date = startdate,
                  end_date = enddate,
                  option_names = option.names,
                  option_values = option.values)
  
  blpDisconnect(conn)
  
  column.names <- c("open", "high", "low", "close", "volume")
  
  dates <- sort(as.Date(unique(bbg.data[, "date"]), format = "%Y-%m-%d"))
    
  stopifnot(class(dates) == "Date")
  
  if(length(tickers) > 1) {  
    
    adj.data <- array(NA,
                      dim = c(length(dates),
                              length(tickers),
                              ncol(bbg.data) - 2),
                      dimnames = list(as.character(dates), 
                                      tickers, 
                                      column.names))
    
    for(i in 1:length(tickers.type)) {
      
      ticker.match <- which(bbg.data[, "ticker"] == tickers.type[i])
      date.match <- match(bbg.data[ticker.match, "date"], as.character(dates))
      
      adj.data[date.match, i, ] <- as.matrix(bbg.data[ticker.match, 3:ncol(bbg.data)])
      
    }
    
  }
  
  if(length(tickers) == 1) {
    
    adj.data <- xts(as.matrix(bbg.data[, -1]), order.by = dates)
    
    colnames(adj.data) <- column.names
    
  }
  
  return(adj.data)
  
}