
#' Function that returns market data (Open, High, Low, Close, Volume, VWAP)
#' @param tickers   a character vector of ticker names (see details)
#' @param type      a character that specifying instrument type (see details)
#' @details The function returns conventional market data such as open,
#'   high, low, close, volume and VWAP. The output object is either a xts
#'   object or array depending on the number of tickers provided (see details).
#' 
#' Tickers are structured as symbols together with the exchange code e.g.
#'   AAPL US for Apple across aggregate US exchanges (composite code) and
#'   AAPL UW for Apple's price data from NASDAQ Global Select
#' 
#' @export
#' @import Rbbg
#' @import xts
#' @examples
#' MarketData(index = "AAPL US", type = "Equity", freq = "DAILY",
#'   currency = NULL, startdate = "20140101", enddate = "20140630",
#'   non.trading.days = "NON_TRADING_WEEKDAYS",
#'   non.trading.days.fill = "NIL_VALUE")

MarketData <- function(tickers = "AAPL US",
                       type = "Equity",
                       freq = "DAILY",
                       currency = NULL,
                       startdate = "20140101",
                       enddate = "",
                       non.trading.days = "NON_TRADING_WEEKDAYS",
                       non.trading.days.fill = "NIL_VALUE") {

  conn <- blpConnect()
  
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
  
  bloomberg.data <- bdh(conn = conn,
                        securities = paste(tickers,
                                           type,
                                           sep = " "),
                        fields = c("PX_OPEN",
                                   "PX_HIGH",
                                   "PX_LOW",
                                   "PX_LAST",
                                   "PX_VOLUME",
                                   "EQY_WEIGHTED_AVG_PX"),
                        start_date = startdate,
                        end_date = enddate,
                        option_names = option.names,
                        option_values = option.values)
  
  blpDisconnect(conn)
  
  if(length(tickers) > 1) {  
    dates <- as.Date(unique(bloomberg.data[, 2]), format = "%Y-%m-%d")
  } else {
    dates <- as.Date(unique(bloomberg.data[, 1]), format = "%Y-%m-%d")
  }  
  
  stopifnot(class(dates) == "Date")
  
  if(length(tickers) > 1) {  
    adj.data <- array(NA,
                      dim = c(length(dates),
                              length(tickers),
                              ncol(bloomberg.data) - 2),
                      dimnames = list(as.character(dates), 
                                      tickers, 
                                      colnames(bloomberg.data[, 3:8])))
    
    for(i in 1:(ncol(bloomberg.data) - 2)) {
      adj.data[,, i] <- matrix(bloomberg.data[, 2 + i],
                               nrow = length(dates),
                               ncol = length(tickers))
    }
    
  }
  
  if(length(tickers) == 1) {
    adj.data <- xts(as.matrix(bloomberg.data[, -1]), order.by = dates)
  }
  
  return(adj.data)
  
}