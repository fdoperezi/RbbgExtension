
#' Function calculating intraday VWAP and extract a VWAP value based
#' on input date and time variable
#' @param data        a list of market bar data in the format created
#'                    from the bar data function
#' @param vwap.date   a character string specifying the date for VWAP
#'                    price point extraction
#' @param vwap.time   a character string specifying the time for VWAP
#'                    price point extraction (see details)
#' @export
#' @import Rbbg
#' @import xts

VwapPoint <- function(data, vwap.date = "2014-04-14",
                      vwap.time = "19:59:00") {
  
  # the function should potentially handle both lists (multiple tickers)
  # and matrix (one ticker)
  
  if(is.list(data) == FALSE) stop("data object is not a list")
  
  list.length <- length(data)
  
  vwap.point <- vector(mode = "numeric", list.length)
  
  col.names <- c("open", "high", "low", "close", "volume")
  
  for(n in 1:list.length) {
    
    if(is.xts(data[[n]]) == FALSE) {
      stop(paste("Component", n, "in the list is not an xts object"))
    }
    
    if(all(col.names) %in% names(data[[1]]) == FALSE) {
      stop("xts object does not have column names:", col.names)
    }    
    
    session.bars <- which(vwap.date == as.Date(.indexDate(data[[n]])))
    
    if(length(session.bars) == 0) {
      
      vwap.point[n] <- NA
      
    } else {
      
      avg.bar.prices <- apply(data[[n]][session.bars, col.names[, 1:4]], 1, mean)
      
      session.cum.volume <- cumsum(data[[n]][session.bars, "volume"])
      
      value.traded.per.bar <- avg.bar.prices * data[[n]][session.bars, "volume"]
      
      session.cum.value.traded <- cumsum(value.traded.per.bar)
      
      vwap.session <- session.cum.value.traded / session.cum.volume
      
      vwap.output.pos <- which(format(as.POSIXct(.index(vwap.session), origin = "1970-01-01", tz = ""), usetz = FALSE) == paste(vwap.date, vwap.time, sep = " "))
      
      vwap.point[n] <- ifelse(length(vwap.output.pos) != 0, vwap.session[vwap.output.pos, ], NA)
      
    }
    
  }
  
  vwap.output <- data.frame(ticker = names(x),
                            vwap = vwap.point,
                            stringsAsFactors = FALSE)
  
  return(vwap.output)
  
}