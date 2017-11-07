#' Calculate effective tax rate
#' 
#' @export
calc_epv <- function(balance_df, income_df, cash_df, interest_rate = 0.03, discount_rate = 0.112) {
  
  # Calculate epv
  # Cash
  cash <- filter_financials_df(balance_df, 'account', c('Cash & Equivalents'))[['value']][[1]]
  
  ## Earnings
  earn <- calc_earn(balance_df, income_df, cash_df)
  
  ### Earnings multiple
  earn.multiple <- 1/discount_rate
  
  #### Earnings power
  ep <- earn*earn.multiple
  
  ##### Earnings power value
  epv <- ep+cash
}