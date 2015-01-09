
#' Function getting historical Bloomberg data
#' @param tickers    character vector specifying the securities
#' @param type       a character string specifying instrument type
#'                   price point extraction
#' @param fields     character vector specifying data fields
#'                   price point extraction (see details)
#' @param freq       a character string specifying time frequency
#' @param currency   a character string specifying the currency
#' @export
#' @import Rbbg
#' @import xts
#' @import zoo

HistData <- function(tickers = "GS US",
                     type = "Equity",
                     fields = "PX_LAST",
                     freq = "DAILY",
                     currency = NULL,
                     startdate = "20140101",
                     enddate = "",
                     non.trading.days = "NON_TRADING_WEEKDAYS",
                     non.trading.days.fill = "NIL_VALUE",
                     calendar.type = "CALENDAR",
                     override.fields = NULL,
                     override.values = NULL) {
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  if(is.null(currency)) {  
    option.names  <- c("periodicitySelection",
                       "nonTradingDayFillOption",
                       "nonTradingDayFillMethod",
                       "periodicityAdjustment",
                       "adjustmentFollowDPDF")
    option.values <- c(freq,
                       non.trading.days,
                       non.trading.days.fill,
                       calendar.type,
                       "TRUE")
  } else {
    option.names <- c("periodicitySelection",
                      "currency",
                      "nonTradingDayFillOption",
                      "nonTradingDayFillMethod",
                      "periodicityAdjustment",
                      "adjustmentFollowDPDF")
    option.values <- c(freq,
                       currency,
                       non.trading.days,
                       non.trading.days.fill,
                       calendar.type,
                       "TRUE")
  }
  
  conn <- blpConnect()
  
  if(length(fields) == 1 & length(tickers) == 1) {
    
    bbg.data <- bdh(conn = conn,
                    securities = tickers.type,
                    fields = fields,
                    start_date = startdate,
                    end_date = enddate,
                    option_names = option.names,
                    option_values = option.values,
                    override_fields = override.fields,
                    override_values = override.values)
    
    dates <- as.Date(unique(bbg.data[, 1]), format = "%Y-%m-%d")
    
    stopifnot(class(dates) == "Date")
    
    adj.data <- xts(as.matrix(bbg.data[, -1],
                              nrow = length(dates),
                              ncol = length(fields),
                              dimnames = list(NULL, fields)),
                    order.by = dates)
    
    colnames(adj.data) <- fields
    
  }
  
  if(length(fields) == 1 & length(tickers) > 1) {
    
    if(calendar.type == "FISCAL") {
      
      adj.data <- vector("list", length(tickers))
      
      names(adj.data) <- tickers
      
      primary.period <- bdp(conn = conn,
                            securities = tickers.type,
                            fields = "PRIMARY_PERIODICITY")
      
      primary.period <- ifelse(str_detect(primary.period[, 1],
                                          "\\bQuarterly\\b"),
                               "QUARTERLY",
                        ifelse(str_detect(primary.period[, 1],
                                          "\\bSemi-Annual\\b"),
                               "SEMI_ANNUALLY",
                               "QUARTERLY"))
      
      for(i in 1:length(tickers)) {
        
        period.sel.pos <- which(option.names == "periodicitySelection")
        
        option.values[period.sel.pos] <- primary.period[i]
        
        bbg.data <- bdh(conn = conn,
                        securities = tickers.type[i],
                        fields = fields,
                        start_date = startdate,
                        end_date = enddate,
                        option_names = option.names,
                        option_values = option.values,
                        override_fields = override.fields,
                        override_values = override.values)
        
        dates <- as.Date(unique(bbg.data[, 1]), format = "%Y-%m-%d")
        
        stopifnot(class(dates) == "Date")
        
        adj.data[[i]] <- xts(matrix(bbg.data[, 2],
                                    nrow = length(dates),
                                    ncol = 1,
                                    dimnames = list(NULL, fields)),
                             order.by = dates)
        
      }
      
    }
    
    if(calendar.type == "CALENDAR" | calendar.type == "ACTUAL") {
      
      bbg.data <- bdh(conn = conn,
                      securities = tickers.type,
                      fields = fields,
                      start_date = startdate,
                      end_date = enddate,
                      option_names = option.names,
                      option_values = option.values,
                      override_fields = override.fields,
                      override_values = override.values)
      
      active.tickers <- bdp(conn = conn,
                            securities = tickers.type,
                            fields = "TRADE_STATUS")
      
      dates <- as.Date(unique(bbg.data[, 2]), format = "%Y-%m-%d")
      
      dates <- sort(dates)
      
      adj.data <- xts(matrix(NA,
                             nrow = length(dates),
                             ncol = length(tickers),
                             dimnames = list(NULL, tickers)),
                      order.by = dates)
      
      for(i in 1:length(tickers)) {
        
        ticker.pos <- which(tickers.type[i] == bbg.data[, "ticker"])
        
        dates.match <- match(bbg.data[ticker.pos, "date"], as.character(dates))
        
        adj.data[dates.match, i] <- bbg.data[ticker.pos, 3]
        
        if(active.tickers[i, "TRADE_STATUS"]) {
          
          adj.data[, i] <- na.locf(adj.data[, i])
          
        } else {
          
          adj.data[, i] <- na.locf(adj.data[, i], fromLast = TRUE, maxgap = 1)
          
        }
        
      }
      
      # This if statement exists due to a date error in Bloomberg's data
      # base where inactive tickers don't have the same date string
      # ...as active tickers creates an error in the unique date method
      # used when all tickers are active 
      if(any(active.tickers[, "TRADE_STATUS"] == FALSE)) {
        
        actv.pos <- which(active.tickers[, "TRADE_STATUS"] == TRUE)[1]
        
        ticker.pos <- which(bbg.data[, "ticker"] == tickers.type[actv.pos])
        
        actv.dates <- as.Date(bbg.data[ticker.pos, "date"],
                              format = "%Y-%m-%d")
        
        true.dates <- match(actv.dates, dates)
        
        adj.data <- adj.data[true.dates, ]
        
      }
      
    }
    
  }
  
  if(length(fields) > 1 & length(tickers) == 1) {
    
    bbg.data <- bdh(conn = conn,
                    securities = tickers.type,
                    fields = fields,
                    start_date = startdate,
                    end_date = enddate,
                    option_names = option.names,
                    option_values = option.values,
                    override_fields = override.fields,
                    override_values = override.values)
    
    dates <- as.Date(unique(bbg.data[, 1]), format = "%Y-%m-%d")
    
    adj.data <- xts(as.matrix(bbg.data[, -1],
                              nrow = length(dates),
                              ncol = length(fields),
                              dimnames = list(NULL, fields)),
                    order.by = dates)
    
  }
  
  if(length(fields) > 1 & length(tickers) > 1) {
    
    if(calendar.type == "FISCAL") {
      
      adj.data <- vector("list", length(tickers))
      
      names(adj.data) <- tickers
      
      primary.period <- bdp(conn = conn,
                            securities = tickers.type,
                            fields = "PRIMARY_PERIODICITY")
      
      primary.period <- ifelse(str_detect(primary.period[, 1],
                                          "\\bQuarterly\\b"),
                               "QUARTERLY",
                        ifelse(str_detect(primary.period[, 1],
                                          "\\bSemi-Annual\\b"),
                               "SEMI_ANNUALLY",
                               "QUARTERLY"))
      
      for(i in 1:length(tickers)) {
        
        period.sel.pos <- which(option.names == "periodicitySelection")
        
        option.values[period.sel.pos] <- primary.period[i]
        
        bbg.data <- bdh(conn = conn,
                        securities = tickers.type[i],
                        fields = fields,
                        start_date = startdate,
                        end_date = enddate,
                        option_names = option.names,
                        option_values = option.values,
                        override_fields = override.fields,
                        override_values = override.values)
        
        if(nrow(bbg.data) > 0 & sum(duplicated(bbg.data[, 1])) == 0) {
          
          dates <- as.Date(unique(bbg.data[, 1]), format = "%Y-%m-%d")
          
          stopifnot(class(dates) == "Date")
          
          col.nas <- apply(is.na(bbg.data), 2, sum)
          
          col.nas.number <- which(col.nas == nrow(bbg.data))
          
          if(length(col.nas.number) > 0) {
            bbg.data[, col.nas.number] <- NA
          }
          
          adj.data[[i]] <- xts(as.matrix(bbg.data[, 2:ncol(bbg.data)],
                                         nrow = length(dates),
                                         ncol = ncol(bbg.data) - 1,
                                         dimnames = list(NULL, fields)),
                               order.by = dates)
          
        } else {
          adj.data[[i]] <- NA
        } 
        
      }
      
    }
    
    if(calendar.type == "CALENDAR" | calendar.type == "ACTUAL") {
      
      bbg.data <- bdh(conn = conn,
                      securities = tickers.type,
                      fields = fields,
                      start_date = startdate,
                      end_date = enddate,
                      option_names = option.names,
                      option_values = option.values,
                      override_fields = override.fields,
                      override_values = override.values)
      
      active.tickers <- bdp(conn = conn,
                            securities = tickers.type,
                            fields = "TRADE_STATUS")
      
      dates <- as.Date(unique(bbg.data[, 2]), format = "%Y-%m-%d")
      
      dates <- sort(dates)
      
      adj.data <- array(NA,
                        dim = c(length(dates),
                                length(tickers),
                                length(fields)),
                        dimnames = list(as.character(dates),
                                        tickers,
                                        fields))
      
      for(i in 1:length(tickers)) {
        
        ticker.pos <- which(tickers.type[i] == bbg.data[, "ticker"])
        
        dates.match <- match(bbg.data[ticker.pos, "date"], as.character(dates))
        
        temp.bbg <- bbg.data[ticker.pos, -1:-2]
        
        adj.data[dates.match, i, ] <- data.matrix(temp.bbg)
        
        if(active.tickers[i, "TRADE_STATUS"]) {
          
          adj.data[, i, ] <- na.locf(xts(adj.data[, i, ], order.by = dates))
          
        } else {
          
          adj.data[, i, ] <- na.locf(xts(adj.data[, i, ], order.by = dates),
                                     fromLast = TRUE,
                                     maxgap = 1)
          
        }
        
      }
      
      # This if statement exists due to a date error in Bloomberg's data
      # base where inactive tickers don't have the same date string
      # as active tickers
      if(any(active.tickers[, "TRADE_STATUS"] == FALSE)) {
        
        actv.pos <- which(active.tickers[, "TRADE_STATUS"] == TRUE)[1]
        
        ticker.pos <- which(bbg.data[, "ticker"] == tickers.type[actv.pos])
        
        actv.dates <- as.Date(bbg.data[ticker.pos, "date"],
                              format = "%Y-%m-%d")
        
        true.dates <- match(actv.dates, dates)
        
        adj.data <- adj.data[true.dates,, ]
        
      }
      
    }
    
  }
  
  blpDisconnect(conn)
  
  return(adj.data)
  
}