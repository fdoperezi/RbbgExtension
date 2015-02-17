
#' Function getting historical point-in-time fundamental data (beta version)
#' @param tickers           character vector specifying the securities
#' @param type              a character string specifying instrument type
#'                          price point extraction
#' @param fields            character vector specifying data fields
#'                          price point extraction (see details)
#' @param override_fields   character vector specifying override fields
#' @param override_values   character vector specifying override values
#' @export
#' @import Rbbg
#' @import xts
#' @import stringr

PointInTime <- function(tickers = "AAPL US",
                        type = "Equity",
                        fields = "SALES_REV_TURN",
                        currency = NULL,
                        startdate = "20090101",
                        enddate = "",
                        sh.out = TRUE) {
  
  conn <- blpConnect()
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  option.names <- "currency"
  option.values <- currency
  
  if(length(tickers.type) == 1 & length(fields) == 1) {
    
    prices <- HistData(tickers = tickers,
                       type = "Equity",
                       fields = c("PX_LAST", "EQY_SH_OUT"),
                       freq = "MONTHLY",
                       startdate = startdate,
                       enddate = enddate)
    
    dates <- as.Date(.indexDate(prices))
    
    pp <- bdp(conn = conn,
              securities = tickers.type,
              fields = "PRIMARY_PERIODICITY")
    
    pp <- ifelse(str_detect(pp[, 1], "\\bQuarterly\\b"), "Q",
                 ifelse(str_detect(pp[, 1], "\\bSemi-Annual\\b"), "S", "Q"))
    
    # Only works with shares outstanding as output
    if(sh.out) {
      
      output <- xts(matrix(NA,
                           nrow = length(dates),
                           ncol = length(fields) + 1,
                           dimnames = list(NULL,
                                           c("EQY_SH_OUT",
                                             fields))),
                    order.by = dates)
      
      output[, "EQY_SH_OUT"] <- as.numeric(prices[, "EQY_SH_OUT"])
      
      for(i in 1:length(dates)) {
        
        output[i, 2:ncol(output)] <- as.numeric(bdp(conn = conn,
                                                    securities = tickers.type,
                                                    fields = fields,
                                                    override_fields = c("FUNDAMENTAL_DATABASE_DATE",
                                                                        "FILING_STATUS",
                                                                        "FUND_PER"),
                                                    override_values = c(gsub("-", "", dates[i]),
                                                                        "OR",
                                                                        pp)))
        
      }
      
    }
    
  }
  
}


