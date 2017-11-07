#' Retrieve financial data denomination
#'
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' 
#' @export
get_financial_denom <- function(symbol) {
  
  temp.url <- sprintf('https://finance.google.com/finance?fstype=ii&q=%s', symbol)
  
  message(temp.url)

  temp.page <- temp.url %>% 
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "nwp", " " ))]') %>% 
    rvest::html_text() %>% stringr::str_extract('In Millions') %>% unique()
  
  if(length(temp.page) < 2) {
    lookup <- c('In Millions' = 1000000)
    
    lookup[[temp.page]]
  }
}