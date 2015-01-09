
#' Function loading all tickers of a specified index
#' @param index   Bloomberg index ticker code
#' @export
#' @import Rbbg
#' @examples
#' IndexMembers(index = "SPX")

IndexMembers <- function(index = "SPX") {
  
  conn <- blpConnect()
  
  index.count <- DataPoint(tickers = index,
                           type = "Index",
                           fields = "COUNT_INDEX_MEMBERS")
  
  if(index.count <= 2500) {
    
    index.members <- bds(conn = conn,
                         securities = paste(index, "Index", sep = " "),
                         fields = "INDX_MEMBERS")
    
  }
  
  if(index.count >= 2500 & index.count <= 5000) {
    
    index.members <- bds(conn = conn,
                         securities = paste(index, "Index", sep = " "),
                         fields = c("INDX_MEMBERS",
                                    "INDX_MEMBERS2"))
    
  }
  
  if(index.count > 5000) {
    
    index.members <- bds(conn = conn,
                         securities = paste(index, "Index", sep = " "),
                         fields = c("INDX_MEMBERS",
                                    "INDX_MEMBERS2",
                                    "INDX_MEMBERS3"))
    
  }
  
  blpDisconnect(conn)
  
  return(index.members[, ncol(index.members)])
  
}
