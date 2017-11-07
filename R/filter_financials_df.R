#' Retrieve financial data
#'
#' @importFrom data.table dcast
#' 
#' @export
filter_financials_df <- function(df, targ_col, targ_colval) {
  
  filter_df <- df[which(df[[targ_col]] %in% targ_colval), ]
  
}
  
  