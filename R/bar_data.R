#' Get historical bar data on financial instruments
#' 
#' The function works for one or multiple tickers across
#' many security classes
#' 
#' @param tickers Character vector specifying the ticker code(s)
#' @param type Specify security class such as equities "Equity" or
#' currencies "Curncy". The input is not case sensitive.
#' @param start.date A character vector specifying the start date in 
#' date class format. See details.
#' @param start.time A character vector specifying the start time.
#' See details.
#' @param end.date A character vector specifying the start date in
#' data class format.
#' @param end.time A character vector.
#' @param time.zone The time zone should be attached to the
#' specified date and time. See details.
#' @param interval A character vector stating the bar interval in 
#' minutes. One minute is the highest frequency allowed.
#' @details In the current version the function has only traded bars as 
#' its default input. Bid and ask bar data points are possible and 
#' will be added in the future.
#' 
#' The date inputs should follow R's Date Class format "\%Y-\%m-\%d" e.g.
#' "2014-01-01" for January 1, 2014. The end date's default value is NULL 
#' which the function interprets as today. Time inputs should the standard 
#' POSIX format "\%H:\%M:\%S" e.g. "09:30:00".
#' 
#' The time zone input is added to make it easier to set times, as the 
#' local time for when the exchange is open never changes, e.g. always 
#' 09:30 to 16:00 for NYSE and NASDAQ unless on a holiday, whereas 
#' GMT changes two times a year because of daylight saving.
#' 
#' The data output is a xts object. For multiple tickers the xts 
#' objects are stored in a list as missing bar data can in some 
#' cases create unequal length of bar data for each ticker making 
#' it difficult to wrap the data into an array. It becomes even more 
#' cumbersome for European equities. The attached time zone is set to 
#' the system's time zone.
#' 
#' As especially stock prices are impacted on ex-dividend dates 
#' the option "adjustmentFollowDPDF" is to TRUE as default in 
#' data query to Bloomberg. The DPDF function in the Bloomberg 
#' terminal is where the user sets "Dividend & Corporate Action 
#' Settings", so if the user has selected adjustment for normal 
#' and abnormal cash dividends (splits) then these adjustments 
#' will be reflected in the output bar data.
#' @export
#' @import Rbbg
#' @import xts
#' @import lubridate
#' @references The official time zone names accepted for POSIX are found 
#' here:
#' \url{http://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
#' @examples
#' BarData()  # Returns six months of 5 minutes bar data on Apple

BarData <- function(tickers = "AAPL US",
                    type = "Equity",
                    start.date = "2014-01-01",
                    start.time = "09:30:00",
                    end.date = NULL,
                    end.time = NULL,
                    time.zone = "America/New_York",
                    interval = "5") {
  
  utc.start <- paste(start.date, start.time, sep = " ")
  utc.start <- paste(format(with_tz(as.POSIXlt(utc.start,
                                               tz = time.zone),
                                    tzone = "GMT"),
                            usetz = FALSE),
                     ".000",
                     sep = "")
  
  if(any(is.null(end.date), is.null(end.time))) {
    
    utc.end <- paste(Sys.Date(), "16:05:00", sep = " ")
    
  } else {
    
    utc.end <- paste(end.date, end.time, sep = "")
    
  }
  
  utc.end <- paste(format(with_tz(as.POSIXlt(utc.end,
                                             tz = time.zone),
                                  tzone = "GMT"),
                          usetz = FALSE),
                   ".000",
                   sep = "")
  
  conn <- blpConnect()
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  if(length(tickers) == 1) {
    
    bbg.data <- bar(conn = conn,
                    security = tickers.type,
                    field = "TRADE",
                    start_date_time = utc.start,
                    end_date_time = utc.end,
                    interval = interval,
                    option_names = "adjustmentFollowDPDF",
                    option_values = "TRUE")
    
    non.volume.bars <- which(bbg.data[, "volume"] == 0)
    
    if(length(non.volume.bars) > 0) {
      
      bbg.data <- bbg.data[-non.volume.bars, ]
      
    }
    
    trade.time <- as.POSIXlt(gsub("T", " ", bbg.data[, "time"]), tz = "GMT")
    
    adj.data <- xts(bbg.data[, -1],
                    order.by = trade.time,
                    tzone = Sys.timezone())
    
    return(adj.data)
    
  } 
  
  if(length(tickers) > 1) {
    
    adj.data <- vector("list", length(tickers))
    
    names(adj.data) <- tickers
    
    for(i in 1:length(tickers)) {
      
      bbg.data <- bar(conn = conn,
                      security = tickers.type[i],
                      field = "TRADE",
                      start_date_time = utc.start.time,
                      end_date_time = utc.end.time,
                      interval = interval,
                      option_names = "adjustmentFollowDPDF",
                      option_values = "TRUE")
      
      non.volume.bars <- which(bbg.data[, "volume"] == 0)
      
      if(length(non.volume.bars) > 0) {
        
        bbg.data <- bbg.data[-non.volume.bars, ]
        
      }
      
      trade.time <- as.POSIXlt(gsub("T", " ", bbg.data[, "time"]), tz = "GMT")
      
      adj.data[[i]] <- xts(as.matrix(bbg.data[, -1]),
                           order.by = trade.time,
                           tzone = Sys.timezone())
      
    }
    
    return(adj.data)
    
  }
  
  blpDisconnect(conn)
  
}