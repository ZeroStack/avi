#' Calculate effective tax rate
#' 
#' @export
#' 
calc_earn <- function(balance_df, income_df, cash_df, interest_rate = 0.03) {
  ###### Calculate earnings
  # EBIT
  sus.ebt <- calc_sus_ebt(income_df)
  
  ## Depreciation adjustment
  dep_adj <- calc_dep_adj(balance_df, income_df, cash_df)
  
  ### Interest on cash
  cash <- filter_financials_df(balance_df, 'account', c('Cash & Equivalents'))[['value']][[1]]
  int.cash <- cash*interest_rate
  
  #### Pretax earnings
  pretax.earn <- (sus.ebt+dep_adj)-int.cash
  
  ##### Effective tax rate
  eff.tax <- calc_eff_tax(income_df)
  exp.tax <- pretax.earn*eff.tax
  
  ###### Earnings
  earn <- pretax.earn-exp.tax
}