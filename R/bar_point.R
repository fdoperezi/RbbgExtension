
#' Function extracting the intraday price based on input date and time
#' on input date and time variable
#' @param data       a list of market bar data in the format created
#'                   from the bar data function
#' @param bar.date   a character string specifying the date for bar
#'                   price point extraction
#' @param bar.time   a character string specifying the time for bar
#'                   price point extraction (see details)
#' @param bar.type   a character string specifying the price point e.g.
#'                   open, high, low, close or volume
#' @export
#' @import Rbbg
#' @import xts

BarPoint <- function(data, bar.date = "2014-04-14", bar.time = "19:58:00",
                     bar.type = "close") {
  
  # the function should potentially handle both lists (multiple tickers)
  # and matrix (one ticker)
  
  if(is.list(data) == FALSE) stop("data object is not a list")
  
  list.length <- length(data)
  
  bar.point <- vector(mode = "numeric", list.length)
  
  for(n in 1:list.length) {
    
    if(is.xts(data[[n]]) == FALSE) {
      stop(paste("Component", n, "in the list is not an xts object"))
    }   
    
    if("close" %in% names(data[[1]]) == FALSE) {
      stop("the xts object does not contain the column name 'close'")
    }    
    
    session.bars <- which(bar.date == as.Date(.indexDate(data[[n]])))
    
    if(length(session.bars) == 0) {
      bar.point[n] <- NA
    } else {
      temp.session.data <- data[[n]][session.bars, ]
      
      bar.output.pos <- which(format(as.POSIXct(.index(temp.session.data),
                                                origin = "1970-01-01", tz = ""),
                                     usetz = FALSE) == paste(bar.date,
                                                             bar.time,
                                                             sep = " "))
      
      bar.point[n] <- ifelse(length(bar.output.pos) != 0,
                             temp.session.data[bar.output.pos, bar.type],
                             NA)
      
    }
    
  }
  
  bar.output <- data.frame(ticker = names(data),
                           price = bar.point,
                           stringsAsFactors = FALSE)
  
  return(bar.output)
  
}