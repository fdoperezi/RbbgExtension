
#' Function getting a single data point
#' @param tickers           character vector specifying the securities
#' @param type              a character string specifying instrument type
#'                          price point extraction
#' @param fields            character vector specifying data fields
#'                          price point extraction (see details)
#' @param override_fields   character vector specifying override fields
#' @param override_values   character vector specifying override values
#' @export
#' @import Rbbg

DataPoint <- function(tickers = "ADS GY", type = "Equity", fields = "PX_LAST",
                      override.fields = NULL, override.values = NULL) {
  
  conn <- blpConnect()
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  bbg.data <- bdp(conn = conn, 
                  securities = tickers.type,
                  fields = fields,
                  override_fields = override.fields,
                  override_values = override.values)
  
  blpDisconnect(conn)
  
  return(bbg.data)
  
}