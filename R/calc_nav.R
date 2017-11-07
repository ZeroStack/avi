#' Calculate Net Asset Value
#'
#' @export
filter_financial_df <- function(df, accounts) {
  
  filter.df <- df[account %in% accounts]
  
  filter.df
}