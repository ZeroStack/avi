#' Screen for stocks
#'
#'@importFrom stringr str_c
#'@importFrom httr GET add_headers
#'@importFrom jsonlite fromJSON
#'@importFrom purrr map
#'@importFrom data.table setDT
#'@importFrom data.table dcast
#'@importFrom data.table as.data.table
#'
#'@export
screen <- function(market_cap = list(), pe_ratio = list(),
                   dividend_yield = list(), 
                   price_change_52week = list(),
                   price_change_26week = list(),
                   price_change_13week = list(),
                   price_to_book = list(),
                   net_profit_margin_percent_trailing_12months = list(),
                   current_assets_to_liabilities_ratio_year = list(),
                   longterm_debt_to_equity_year = list(),
                   return_on_equity_5years = list(),
                   net_income_growth_rate_5years = list(),
                   revenue_growth_rate_10years = list(),
                   eps_growth_rate_10years = list(),
                   interest_coverage_year = list(),
                   max_results = 1300
) {
  
  #Lookup for temp.url
  lookup <- c('[' = '%5B', ']' = '%5D', '(' = '%28', ')' = '%29', '>' = '>', '<' = '<', '=' = '%3D', '&' = '%26')
  
  max_results <- max_results + round(rnorm(1, mean = 10),0)
  
  # Build the json request
  temp.url <- stringr::str_c("https://finance.google.com/finance?start=1&num=30&q=",
                    #open square bracket
                    lookup[['[']],
                    stringr::str_c(formulate_criteria('last_price', list(0, 300000), lookup),
                                   formulate_criteria('market_cap', market_cap, lookup),
                                   formulate_criteria('pe_ratio', pe_ratio, lookup),
                                   formulate_criteria('net_profit_margin_percent_trailing_12months', net_profit_margin_percent_trailing_12months, lookup),
                                   formulate_criteria('dividend_yield', dividend_yield, lookup),
                                   formulate_criteria('price_change_52week', price_change_52week, lookup),
                                   formulate_criteria('price_change_26week', price_change_26week, lookup),
                                   formulate_criteria('price_change_13week', price_change_13week, lookup),
                                   formulate_criteria('price_to_book', price_to_book, lookup),
                                   formulate_criteria('current_assets_to_liabilities_ratio_year', current_assets_to_liabilities_ratio_year, lookup),
                                   formulate_criteria('longterm_debt_to_equity_year', longterm_debt_to_equity_year, lookup),
                                   formulate_criteria('return_on_equity_5years', return_on_equity_5years, lookup),
                                   formulate_criteria('net_income_growth_rate_5years', net_income_growth_rate_5years, lookup),
                                   formulate_criteria('revenue_growth_rate_10years', revenue_growth_rate_10years, lookup),
                                   formulate_criteria('eps_growth_rate_10years', eps_growth_rate_10years, lookup),
                                   formulate_criteria('interest_coverage_year', interest_coverage_year, lookup),
                                   sep = str_c('+', lookup[['&']], '+')
                    ),
                    #closing square bracket
                    lookup[[']']],
                    '&restype=company&noIL=1&num=', max_results ,'&output=json&ei=57VwWOnoM4mi0wTw_oagBA'
                    
  )
  message(temp.url)
  
  # request the data
  url.data <- try(httr::GET(temp.url, add_headers("user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36", "Cache-Control" = "no-cache")))
  
  url.extract <- gsub("\\", "", url.data, fixed = TRUE)
  
  url.parse <- jsonlite::fromJSON(url.extract, flatten=TRUE)
  
  stocks.result <- as.numeric(url.parse[['num_company_results']])
  
  stocks <- url.parse[['searchresults']]
    
  message(paste0(stocks.result), ' Stocks retrieved')
  
  if(stocks.result > 0) {
    for(s in 1:stocks.result) {
      
      temp.symbol <- paste0(stocks[s,][['exchange']], ":", stocks[s,][['ticker']])
      
      temp.data <- stocks[s,][['columns']][[1]][,c('field', 'value')]
      
      if(is.null(temp.data)) {
        next
      } else {
        
        temp.data <- data.table::setDT(temp.data) 
        
        temp.data <- temp.data %>% data.table::dcast(formula=.~field)
        temp.data[['symbol']] <- temp.symbol
        
        if(!(exists('opened.columns'))) {
          opened.columns <- temp.data
          
        } else {
          opened.columns <- rbind(opened.columns, temp.data)
        }
      }
    }
  }
  
  stocks[['symbol']] <- paste0(stocks[['exchange']], ':', stocks[['ticker']])
  
  opened.columns[['.']] <- NULL
  
  stocks[['columns']] <- NULL
  
  stocks <- data.table::as.data.table(stocks)[opened.columns, on = 'symbol']
  
  numeric.columns <- c('AINTCOV', 'CurrentRatioYear', 
                       'DividendYield', 'EPSGrowthRate10Years', 
                       'LTDebtToEquityYear', 'NetIncomeGrowthRate5Years', 
                       'NetProfitMarginPercent', 'PE', 'Price52WeekPercChange', 
                       'Price26WeekPercChange', 'Price13WeekPercChange',
                       'PriceToBook', 'QuoteLast', 'ReturnOnEquity5Years', 
                       'RevenueGrowthRate10Years')
  
  stock.columns <- names(stocks)
  
  for(col in numeric.columns) {
    if(col %in% stock.columns) {
      stocks[[col]] <- as.numeric(stocks[[col]])
    }
  }
  
  stocks[['unicorn']] <- as.numeric(str_detect(stocks[['MarketCap']], 'B'))
  
  stocks[['millenial']] <- as.numeric(str_detect(stocks[['MarketCap']], 'M'))
  
  stocks[['trillenial']] <- as.numeric(str_detect(stocks[['MarketCap']], 'T'))
  
  stocks[['trust']] <- as.numeric(str_detect(stocks[['title']], 'Trust'))
  
  stocks[['fund']] <- as.numeric(str_detect(stocks[['title']], 'Fund'))
  
  stocks[['investment']] <- as.numeric(str_detect(stocks[['title']], 'Investment'))
  
  stocks[['bank']] <- as.numeric(str_detect(stocks[['title']], 'Bank'))
  
  stocks[['bancorp']] <- as.numeric(str_detect(stocks[['title']], 'Bancorp'))
  
  stocks[['capital']] <- as.numeric(str_detect(stocks[['title']], 'Capital'))
  
  stocks[['financial']] <- as.numeric(str_detect(stocks[['title']], 'Financial'))
  
  stocks[['etf']] <- as.numeric(str_detect(stocks[['title']], 'ETF'))
  
  stocks[['MarketCapBn']] <- 0
  
  for(row in 1:nrow(stocks)) {
    temp.cap <- stocks[row,][['MarketCap']]

    denom.cap <- str_extract(temp.cap, 'B|M|T')

    lookup.cap <- c('B' = 1000000000, 'M' = 1000000, 'T' = 1000000000000)

    denom.cap <- lookup.cap[denom.cap]
    
    if(is.na(denom.cap)) {
      denom.cap <- 1
    }

    stocks[row,][['MarketCapBn']] <- (as.numeric(str_extract(temp.cap, '\\d+\\.*\\d*'))*denom.cap)/1000000000

  }
  return(stocks)
}


