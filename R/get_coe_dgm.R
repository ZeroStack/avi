#' Calculate equity risk premium
#' 
#' @importFrom quantmod getQuote
#' 
#' @export
get_coe_dgm <- function(symbol) {
  
  income.df <- get_financials_df(symbol, type = 'IS')
  
  dividend.df <- filter_financials_df(income.df, 'account', c('Dividends per Share - Common Stock Primary Issue'))
  
  dividend.share <<- dividend.df[['value']][[1]]
  
  if(identical(dividend.share, 0)) {
    # There is no dividend data
    return(invisible(NULL))
  } else {
    # Last price
    price.share <<- quantmod::getQuote(symbol, src = 'yahoo')[['Last']]
    dividend.yield <- (dividend.share/price.share)
    
    income.net <- filter_financials_df(income.df, 'account', 'Net Income')[['value']][[1]]
    
    balance.df <- get_financials_df(symbol, type = 'BS')
    
    balance.equity <- filter_financials_df(balance.df, 'account', 'Total Equity')
    
    balance.equity.mean <- mean(balance.equity[['value']][1:2])
    
    return.equity <- income.net/balance.equity.mean
    
    cash.df <- get_financials_df(symbol, type = 'CF')
    
    cash.dividends <- -filter_financials_df(cash.df, 'account', 'Total Cash Dividends Paid')[['value']][[1]]
    
    payout.ratio <- cash.dividends/income.net
    
    rev.payout.ratio <- 1 - payout.ratio
    
    growth.expected <<- rev.payout.ratio*return.equity
    
    cost.of.equity <- ((dividend.share*(1+growth.expected))/price.share)+growth.expected
    
  }
  
}