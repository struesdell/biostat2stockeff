get_lengthImputationsTab <- function(){
  
  cn <- c('SPECIES_ITIS', 'STOCK_ABBREV', 'SEX_TYPE', 'ASSESSMENT_ABBREV',
          'SA_YEAR', 'YEAR', 'NESPP4', 'REGION_ID', 'BLOCK_ID', 'LENGTH_UOM',
          'LENGTH', 'NO_AT_LENGTH')
  
  out <- lapply(cn, function(x) tibble('') %>% 
                  rename_with(~x)) %>%
    bind_cols()
  
}