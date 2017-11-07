#' Calculate beta
#'

#' @export
calc_beta <- function(symbol, benchmark = 'SPY') {
  
  symbol.data <- get_symbol_df(symbol)
  
  symbol.ret.col <- paste0(symbol, '.ret')
  
  benchmark.ret.col <- paste0(benchmark, '.ret')
  
  symbol.data <- symbol.data[, c("date","date_parse", symbol.ret.col), with = FALSE]
  
  benchmark.data <- get_symbol_df(benchmark)
  
  benchmark.data <- benchmark.data[, c('date', benchmark.ret.col) , with = FALSE]
  
  cov.data <- symbol.data[benchmark.data, on='date']
  
  cov.data <- cov.data[complete.cases(cov.data),]
  
  beta <- cov(cov.data[[symbol.ret.col]], cov.data[[benchmark.ret.col]])/var(cov.data[[benchmark.ret.col]])
  
}