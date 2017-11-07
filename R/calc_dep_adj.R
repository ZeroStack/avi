
#' Calculate depreciation adjustment
#'
#' 
#' @export
calc_dep_adj <- function(balance_df, income_df, cash_df ) {
  
  depreciation <- filter_financials_df(cash_df, 'account', c('Depreciation/Depletion', 'Amortization'))[['value']][[1]]
  
  property <- filter_financials_df(balance_df, 'account', c('Property/Plant/Equipment, Total - Gross'))[['value']][[1]]
  
  revenue <- filter_financials_df(income_df, 'account', c('Total Revenue'))
  
  recent.revenue <- revenue[['value']][[1]]
  prior.revenue <- revenue[['value']][[2]]
  
  change.revenue <- recent.revenue-prior.revenue
  
  if(change.revenue < 0) {
    return(depreciation)
  } else {
    capex <- filter_financials_df(cash_df, 'account', c('Capital Expenditures'))[['value']][[1]]
    
    growth.capex <- (property/recent.revenue)*change.revenue
    
    zero.growth.capex <- -capex-growth.capex
    
    dep_adj <- depreciation-zero.growth.capex
    
    return(dep_adj)
  }
}