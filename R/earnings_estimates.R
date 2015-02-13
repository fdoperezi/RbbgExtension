
#' Function getting current Bloomberg estimates
#' @param tickers character vector specifying the securities
#' @param type a character string specifying instrument type
#' price point extraction
#' @param fields character vector specifying data fields
#' price point extraction (see details)
#' @param currency a character string specifying the currency
#' @details The function finds and implement the company's
#' primary periodicity so the company's estimates are either
#' quarterly, semi-annually or annually depending on the
#' available period.
#' @export
#' @import Rbbg
#' @import stringr

BEst <- function(tickers = "GS US",
                 type = "Equity",
                 fields = "BEST_EPS",
                 currency = NULL) {
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  conn <- blpConnect()
  
  earn.est <- bdp(conn = conn,
                  securities = tickers.type,
                  fields = c("BEST_EEPS_CUR_QTR",
                             "BEST_EEPS_CUR_SEMI",
                             "BEST_EEPS_CUR_YR"))
  
  fperiod.override <- ifelse(!is.na(earn.est[, 1]), "1FQ",
                             ifelse(!is.na(earn.est[, 2]), "1FS", "1FY"))
  
  override.fields <- c("BEST_FPERIOD_OVERRIDE")
  
  if(!is.null(currency)) {
    
    override.fields <- c(override.fields, "EQY_FUND_CRNCY")
    
  }
  
  if(length(tickers) == 1) {
    
    adj.data <- bdp(conn = conn,
                    securities = tickers.type,
                    fields = fields,
                    override_fields = override.fields,
                    override_values = c(fperiod.override,
                                        currency))
    
    adj.data$PERIOD <- fperiod.override
    
    rownames(adj.data) <- tickers
    
  } else {
    
    adj.data <- matrix(NA,
                       nrow = length(tickers),
                       ncol = length(fields),
                       dimnames = list(tickers,
                                       fields))
    
    for(i in 1:length(tickers)) {
      
      bbg.data <- bdp(conn = conn,
                      securities = tickers.type[i],
                      fields = fields,
                      override_fields = override.fields,
                      override_values = c(fperiod.override[i],
                                          currency))
     
      adj.data[i, ] <- as.numeric(bbg.data)
      
    }
    
    adj.data <- as.data.frame(adj.data)
    
    adj.data$PERIOD <- fperiod.override
    
  }
  
  blpDisconnect(conn)
 
  return(adj.data)
  
}
