#' Retrieve industry and sector
#'
#' @importFrom RCurl getURIAsynchronous 
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_extract str_replace
#' @importFrom parallel mclapply detectCores makeCluster
#' 
#' @export
get_industries <- function(symbols) {

  # Setting up parallel backend
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-1)
  ############################# EOF setup

  symbols.len <- length(symbols)

  corpi <- parallel::mclapply(symbols, function(symbol) {
    
    temp.url <- sprintf('https://finance.google.com/finance?&q=%s', symbol)
    
    temp.page <- try(temp.url %>% 
                       RCurl::getURIAsynchronous() %>% 
                       xml2::read_html() %>%
                       rvest::html_nodes(xpath='//*[@id="related"]/div[4]/div/div[1]') %>% 
                       rvest::html_text() %>% 
                       stringr::str_extract('Industry:.*') %>%
                       stringr::str_replace('\n', ""))
    
    
    if(class(temp.page) == 'try-error' | identical(temp.page, character(0))) {
      return(NA)
    } else {
      return(temp.page)
    }
  }, mc.cores=2)


}