get_prorateTab <- function(yrs, specs, yrsN = NULL){
  
  out <- tibble(
    SPECIES_ITIS = specs$species_itis,
    STOCK_ABBREV = specs$stock_abbrev,
    SEX_TYPE = specs$sex_type,
    ASSESSMENT_ABBREV = specs$assessment_abbrev,
    SA_YEAR = specs$sa_year,
    YEAR = sort(yrs),
    PRORATE_COMBINED = 'Y')
  
  if(!is.null(yrsN)){
    
    out <- out %>%
      mutate(PRORATE_COMBINED = ifelse(YEAR %in% yrsN, 'N', 'Y'))
    
  }
  
  return(out)
  
}