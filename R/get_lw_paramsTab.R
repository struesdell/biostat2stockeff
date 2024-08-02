get_lw_paramsTab <- function(bsdat){
  
  out <- tibble(
    SPECIES_ITIS = species_itis,
    STOCK_ABBREV = stock_abbrev,
    SEX_TYPE = sex_type,
    ASSESSMENT_ABBREV = assessment_abbrev,
    SA_YEAR = sa_year,
    ALPHA = 0.000014041,
    BETA = 3.1693,
    SOURCE = paste('Morse (1978) NMFS NEFC Sandy Hook Lab Tech Ser',
                   'Rep. No 12. 41 p.'),
    LW_TYPE = 'ANNUAL',
    LW_ID = 1)
  
  return(out)
  
}