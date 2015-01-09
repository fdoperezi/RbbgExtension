
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

PointInTime <- function(tickers = "AAPL US",
                        type = "Equity",
                        fields = "TRAIL_12M_EPS",
                        currency = NULL,
                        startdate = "20050101", 
                        enddate = "") {
  
  conn <- blpConnect()
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  option.names <- "currency"
  option.values <- currency
  
  if(length(tickers.type) == 1 & length(fields) == 1) {
    
    earn.dates <- bds(conn = conn,
                      securities = tickers.type,
                      fields = "ERN_ANN_DT_AND_PER")
    
    pp <- bdp(conn = conn,
              securities = tickers.type,
              fields = "PRIMARY_PERIODICITY")
    
    pp <- ifelse(str_detect(pp[, 1], "\\bQuarterly\\b"), "Q",
                 ifelse(str_detect(pp[, 1], "\\bSemi-Annual\\b"), "S", "Q"))
    
    earn.dates <- earn.dates[grep("Q", earn.dates[, 2]), ]
    
    dates <- as.Date(earn.dates[, 1], format = "%Y-%m-%d")
    startdate.adj <- as.Date(startdate, format = "%Y%m%d")
    output.dates <- which(dates > startdate.adj)
    
    earn.dates <- earn.dates[output.dates, ]
    earn.dates <- earn.dates[order(earn.dates[, 1]), ]
    
    sort.dates <- sort((dates[output.dates]))
    
    output <- xts(matrix(NA,
                         nrow = nrow(earn.dates),
                         ncol = length(fields),
                         dimnames = list(NULL,
                                         fields)),
                  order.by = sort.dates)
    
    for(i in 1:nrow(earn.dates)) {
      
      output[i, ] <- bdp(conn = conn,
                         securities = tickers.type,
                         fields = fields,
                         override_fields = c("FUNDAMENTAL_DATABASE_DATE",
                                             "FILING_STATUS",
                                             "FUND_PER"),
                         override_values = c(gsub("-", "", sort.dates[i] + 1),
                                             "OR",
                                             pp))
      
      
    }
    
  }
  
}


