#' Calculate annualized return of a symbol
#' 
#' @importFrom quantmod getSymbols
#' 
#' @export
get_annualize_ret <- function(symbol = '^GSPC') {
  
  spx <- getSymbols(symbol,
                    auto.assign = FALSE, 
                    from = "1950-01-01", 
                    src='yahoo',
                    return.class = 'data.frame')
  
  spx.rows <- nrow(spx)
  
  dates <- as.Date(rownames(spx), '%Y-%m-%d')
  
  first.date <- dates[[1]]
  last.date <- dates[[spx.rows]]
  
  cumulative.ret <- (spx[['GSPC.Close']][[spx.rows]]-spx[['GSPC.Close']][[1]])/spx[['GSPC.Close']][[1]]
  
  annualize.ret <- ((1+cumulative.ret)^(365/as.numeric(difftime(last.date, first.date, units = 'days'))))-1
}