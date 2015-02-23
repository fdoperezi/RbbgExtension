
#' Function getting historical index tickers to avoid surviourship bias
#' @param index       a character string specifying the index
#' @param startdate   a character string specifying the start date
#' @param enddate     a character string specifying the end date
#' @param freq        a character string specifying time frequency
#' @export
#' @import Rbbg
#' @import xts

HistIndexTickers <- function(index = "SPX",
                             startdate = "20090101",
                             enddate = "",
                             freq = "MONTHLY") {
  
  conn <- blpConnect()
  
  index.prices <- HistData(tickers = index,
                           type = "Index",
                           fields = "PX_LAST",
                           freq = freq,
                           startdate = startdate,
                           enddate = enddate)
  
  date.seq <- as.Date(.indexDate(index.prices), origin = "1970-01-01")
  
  hist.index.tickers.temp <- vector("list", length(date.seq))
  
  print(paste("Finding historical tickers for", index, "on...", sep = " "))
  
  for(i in 1:length(date.seq)) {
    
    cat(paste(date.seq[i]), "\n")
    
    bbg.data <- bds(conn = conn,
                    securities = paste(index, "Index", sep = " "),
                    fields = "INDX_MWEIGHT_HIST",
                    override_fields = "END_DATE_OVERRIDE",
                    override_values = gsub("[[:punct:]]","",date.seq[i]))
    
    composite.ticker <- bdp(conn = conn,
                            securities = paste(bbg.data[, "Index Member"], "Equity"),
                            fields = "COMPOSITE_EXCH_CODE")
    
    hist.index.tickers.temp[[i]] <- paste(substr(bbg.data[, 1],
                                                 1,
                                                 nchar(bbg.data[, 1]) - 2),
                                          composite.ticker[, 1],
                                          sep = "")

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