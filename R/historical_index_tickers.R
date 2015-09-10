#' Get historical index tickers to avoid surviourship bias
#' 
#' The function works for one equity index at a time
#' 
#' @param index a character string specifying the index ticker
#' @param field specifying the output instrument identification
#' @param startdate   a character string specifying the start date
#' @param enddate     a character string specifying the end date
#' @param freq        a character string specifying time frequency
#' @details The field variable has two options: "ticker" or "isin" 
#' enabling the function to return the desired instrument identification.
#' 
#' The frequency variable is set using the standard Bloomberg inputs such 
#' as "MONTHLY", "DAILY" etc.
#' @export
#' @import Rbbg
#' @import xts

HistIndexTickers <- function(index = "SPX",
                             field = "ticker",
                             startdate = "20140101",
                             enddate = "",
                             freq = "MONTHLY") {
  
  conn <- blpConnect()
  
  field.df <- data.frame(ticker = "COMPOSITE_EXCH_CODE",
                         isin = "ID_ISIN",
                         stringsAsFactors = FALSE)
  
  index.prices <- HistData(tickers = index,
                           type = "Index",
                           fields = "PX_LAST",
                           freq = freq,
                           startdate = startdate,
                           enddate = enddate)
  
  date.seq <- index(index.prices)
  
  if(freq == "QUARTERLY") date.seq <- as.Date(index(index.prices), frac = 1)
  if(freq == "MONTHLY") date.seq <- as.Date(index(index.prices), frac = 1)
  
  hist.index.tickers.temp <- vector("list", length(date.seq))
  
  print(paste("Finding historical tickers for", index, "on...", sep = " "))
  
  for(i in 1:length(date.seq)) {
    
    cat(paste(date.seq[i]), "\n")
    
    bbg.data <- bds(conn = conn,
                    securities = paste(index, "Index", sep = " "),
                    fields = "INDX_MWEIGHT_HIST",
                    override_fields = "END_DATE_OVERRIDE",
                    override_values = gsub("[[:punct:]]","",date.seq[i]))
    
    field.data <- bdp(conn = conn,
                      securities = paste(bbg.data[, "Index Member"], "Equity"),
                      fields = field.df[, field])
    
    if(field == "ticker") {
      
      hist.index.tickers.temp[[i]] <- paste(substr(bbg.data[, 1],
                                                   1,
                                                   nchar(bbg.data[, 1]) - 2),
                                            field.data[, 1],
                                            sep = "")
      
    }
    
    if(field == "isin") {
      
      hist.index.tickers.temp[[i]] <- field.data[, 1]
      
    }

  }
  
  unique.tickers <- unique(unlist(hist.index.tickers.temp))
  
  hist.index.tickers <- xts(matrix(NA,
                                   nrow = length(date.seq),
                                   ncol = length(unique.tickers),
                                   dimnames = list(NULL, unique.tickers)),
                            order.by = date.seq)
  
  for(i in 1:length(date.seq)) {
    
    index.pos <- match(hist.index.tickers.temp[[i]], unique.tickers)
    
    hist.index.tickers[i, index.pos] <- 1
    
  }
  
  return(hist.index.tickers)
  
}