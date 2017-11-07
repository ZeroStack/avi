#' Retrieve financial data
#' 
#' @importFrom data.table setDT
#' @importFrom data.table setnames
#' @importFrom data.table melt
#' @importFrom quantmod getFinancials
#' @importFrom quantmod viewFinancials
#' @importFrom data.table key
#' 
#' @export
get_financials_df <- function(symbol, type, period = 'A') {
  temp.data <- quantmod::viewFinancials(quantmod::getFinancials(symbol, src = 'google', env = NULL), 
                                        type = type,
                                        period = period
                                        )
  
  temp.data <- data.table::setDT(as.data.frame(temp.data),
                                 keep.rownames=TRUE)
  
  temp.data <- data.table::setnames(temp.data, "rn", "account")
  
  temp.data <- data.table::melt(temp.data, id.vars = 'account',
                  variable.name = 'date',
                  value.name = 'value'
                  )
  
  temp.data <- temp.data[, index := .GRP, by = list(account)]
  
  temp.data <- temp.data[, date_parse := lubridate::parse_date_time(date, '%Y-%m-%d'),]
  
  temp.data <- temp.data[order(-temp.data$date_parse , temp.data$index)]
  
  temp.data[is.na(temp.data)] <- 0
   
  temp.data
}