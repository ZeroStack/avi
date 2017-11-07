#' Retrieve long term interest rate
#' 
#' importFrom Quandl Quandl
#' 
#' @export
get_bond_riskf <- function(api_key="DTynS_LtABNwms7mxX_y", treasury="LT Composite > 10 Yrs") {
  
  interest.data <- Quandl::Quandl("USTREASURY/LONGTERMRATES", api_key=api_key)
  
  interest.data[[treasury]][[1]]/100
}