#'
#'
#'@export
formulate_criteria <- function(criteria_name, criteria, lookup) {
  
  options(scipen = 999)
  # temporary environment
  formulateEnv <- new.env()
  
  # Assign switch
  assign(x = 'i',
         value = 1,
         envir = formulateEnv
  )
  
  temp <- purrr::map(criteria, function(x) {
    
    # Get i
    i <- get(x = 'i',
             envir= formulateEnv
    )
    
    if(i == 1) {
      
      operator <- lookup[['>']]
      
      assign(x = 'i',
             value = 2,
             envir = formulateEnv
      )
    } else {
      
      operator <- lookup[['<']]
    }
    
    stringr::str_c(lookup[['(']], criteria_name, '+', operator , lookup[['=']], '+',  x, lookup[[')']] )
  })
  
  rm(formulateEnv)
  temp <- str_c(temp, collapse = stringr::str_c('+', lookup[['&']], '+') )
}