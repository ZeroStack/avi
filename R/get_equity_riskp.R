#' Calculate equity risk premium
#' 
#' @export
get_equity_riskp <- function(symbol) {
  
  market.return <- get_annualize_ret("^GSPC")
  
  risk.free <- get_bond_riskf()
  
  equity.beta <- calc_beta(symbol)
  
  equity.riskp <- risk.free + equity.beta*(market.return-risk.free)
  
}
