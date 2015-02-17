
#' Function getting historical point-in-time fundamental data
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
                        startdate = "20050101",
                        enddate = "",
                        sh.out = FALSE) {
  
  conn <- blpConnect()
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  option.names <- "currency"
  option.values <- currency
  
  if(length(tickers.type) == 1 & length(fields) == 1) {
    
    #earn.dates <- bds(conn = conn,
    #                  securities = tickers.type,
    #                  fields = "ERN_ANN_DT_AND_PER")
    
    #This bulk data field returns a list of adjustment factors used to adjust prices and/or volumes for 
    #stock splits, stock dividends, rights issues and spin-offs. Adjustment factors for normal and 
    #abnormal cash dividends are not included.
    #The field returns data in 4 unlabeled columns:
    #Column 1 - Adjustment Date
    #Column 2 - Adjustment Factor
    #Column 3 - Operator Type (1=div, 2=mult, 3=add, 4=sub. Opposite for Volume)
    #Column 4 - Flag (1=prices only, 2=volumes only, 3=prices and volumes)
    
    hist.adjust <- bds(conn = conn,
                       securities = tickers.type,
                       fields = "EQY_DVD_ADJUST_FACT")
    
    prices <- HistData(tickers = tickers,
                       type = "Equity",
                       fields = "PX_LAST",
                       freq = "MONTHLY",
                       startdate = startdate,
                       enddate = enddate)
    
    dates <- as.Date(.indexDate(prices))
    

    
    pp <- bdp(conn = conn,
              securities = tickers.type,
              fields = "PRIMARY_PERIODICITY")
    
    pp <- ifelse(str_detect(pp[, 1], "\\bQuarterly\\b"), "Q",
                 ifelse(str_detect(pp[, 1], "\\bSemi-Annual\\b"), "S", "Q"))
    
    output <- xts(matrix(NA,
                         nrow = length(dates),
                         ncol = length(fields),
                         dimnames = list(NULL,
                                         fields)),
                  order.by = dates)
    
    "BS_SH_OUT"
    
    for(i in 1:length(dates)) {
      
      output[i] <- as.numeric(bdp(conn = conn,
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


