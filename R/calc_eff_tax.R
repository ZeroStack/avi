#' Calculate effective tax rate
#'
#' 
#' @export
calc_eff_tax <- function(income_df) {
 
  tax <- filter_financials_df(income_df, 'account', c('Income Before Tax', 'Income After Tax'))
  
  tax <- data.table::dcast(tax, date+date_parse ~ account)
  
  tax <- tax[, eff_tax := `Income After Tax`/`Income Before Tax`]
  
  tax <- 1-mean(tax[['eff_tax']])
}