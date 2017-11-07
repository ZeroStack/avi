#' Retrieve industry and sector
#'
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' 
#' @export
get_industry <- function(symbol) {
  
  temp.url <- sprintf('https://finance.google.com/finance?&q=%s', symbol)
  
  message(temp.url)
  
  temp.page <- try(temp.url %>%
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
}

