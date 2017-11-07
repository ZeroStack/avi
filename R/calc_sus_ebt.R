
#' Calculate expected sustainable earnings before tax
#'
#' @importFrom data.table dcast
#' @importFrom data.table is.data.table
#' @importFrom lubridate parse_date_time
#' 
#' @export
calc_sus_ebt <- function(df) {
  
  income <- filter_financials_df(df, 'account', c('Total Revenue', 'Operating Income'))
  
  income <- data.table::dcast(income, date+date_parse ~ account)
  
  print(is.data.table(income))
  
  income <- income[, ebt_margin := `Operating Income`/`Total Revenue`]

  mean.ebt.margin <- mean(income[['ebt_margin']])

  sus.ebt <- income[['Total Revenue']][[1]]*mean.ebt.margin
  
  sus.ebt
}


