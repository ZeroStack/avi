#' Calculate growth value
#' 
#' @export
calc_gv <- function(balance_df, income_df, cash_df, cost_of_equity = 0.112) {
  
  # Calculat growth value
  # Earnings
  earn <- calc_earn(balance_df, income_df, cash_df)
  
  ## Net asset value
  nav <- filter_financials_df(balance_df, 'account', c('Total Assets'))[['value']][[1]]
  
  ### Return on net asset value
  return.nav <- earn/nav
  
  #### Growth multiple
  growth.multi <- return.nav/cost_of_equity
  
  ##### Earnings power value
  epv <- calc_epv(balance_df, income_df, cash_df)
  
  ###### Growty value
  gv <- epv*growth.multi
} 