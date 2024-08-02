get_sex_typeTab <- function(){
  
  out <- tibble(
    SPECIES_ITIS = species_itis,
    STOCK_ABBREV = stock_abbrev,
    ASSESSMENT_ABBREV = assessment_abbrev,
    SA_YEAR = sa_year,
    CF_START_YEAR = NA_integer_,
    CF_END_YEAR = NA_integer_,
    SEX_TYPE = 'NONE')
  
  return(out)
  
}