#' Putting it altogether to retrieve Applied Value Investing Metrics
#' 
#' @importFrom data.table rbindlist
#' 
#' @export
get_avi <- function(symbol) {
  
  balance.df <- get_financials_df(symbol, 'BS')
  
  cash.df <- get_financials_df(symbol, 'CF')
  
  income.df <- get_financials_df(symbol, 'IS')
  
  # Cost of equity
  equity_riskp <- get_equity_riskp(symbol)
  
  # Interest rate
  interest.rate <- get_bond_riskf()
  
  # Net Assset Value
  nav <- filter_financials_df(balance.df, 'account', c('Total Assets'))[['value']][[1]]
  
  # Earnings Power Value
  epv <- calc_epv(balance.df, income.df, cash.df, interest_rate = interest.rate, discount_rate = equity_riskp)
  
  gv <- calc_gv(balance.df, income.df, cash.df, cost_of_equity = equity_riskp)
  
  avi.df <- data.frame(symbol, nav, epv, gv)
  
  names(avi.df) <- c('Symbol', 'NAV', 'EPV', 'GV')
  
  denomination <- get_financial_denom(symbol)
  
  shares.outstanding <- filter_financials_df(balance.df, 'account', 'Total Common Shares Outstanding')[['value']][[1]]
  
  #shares.outstanding <- shares.outstanding*denomination
  avi.df[,2:4] <- (avi.df[,2:4])/shares.outstanding
  
  avi.df
}