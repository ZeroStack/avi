#' Calculate beta
#'
#' @importFrom data.table setDT
#' @importFrom data.table setnames
#' @importFrom lubridate parse_date_time
#' @importFrom TTR ROC
#' @importFrom quantmod getSymbols
#' 
#' @export
get_symbol_df <- function(symbol) {
  
  symbol.data <- getSymbols(symbol, src = 'google', env = NULL, from="2007-01-01")
  
  symbol.data <- data.table::setDT(x = as.data.frame(symbol.data), 
                                   keep.rownames = TRUE
                                   )
  
  symbol.data <- data.table::setnames(symbol.data, "rn", "date")
  
  symbol.data <- symbol.data[, date_parse := lubridate::parse_date_time(date, '%Y-%m-%d'),]
  
  symbol.data <- symbol.data[order(symbol.data$date_parse)]
  
  symbol.data[[paste0(symbol,'.ret')]] <- TTR::ROC(symbol.data[[paste0(symbol,'.Close')]])
  
  symbol.data
}