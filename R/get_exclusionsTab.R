get_exclusionsTab <- function(){
  
  cn <- c('SPECIES_ITIS', 'STOCK_ABBREV', 'SEX_TYPE', 'ASSESSMENT_ABBREV',
          'SA_YEAR', 'YEAR', 'NESPP4', 'AREA', 'PORT', 'MONTH', 'DAY',
          'CATDISP', 'SEX', 'LINK', 'BIOSAMP_SOURCE', 'LENGTH', 'LENGTH_UOM',
          'AGESTRCT', 'AGE', 'AGE_UOM', 'REASON')
  
  out <- lapply(cn, function(x) tibble('') %>% 
                  rename_with(~x)) %>%
    bind_cols()
  
}