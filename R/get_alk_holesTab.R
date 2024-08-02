get_alk_holesTab <- function(alkmoddf, blkdf, meta){
  
  uMkt <- alkmoddf %>%
    pull(NESPP4) %>%
    unique()
  
  brSpecs <- blkdf %>%
    select(NESPP4, REGION_ID, BLOCK_ID) %>%
    distinct()
  
  out <- alkmoddf %>%
    mutate(SPECIES_ITIS = meta$species_itis,
           STOCK_ABBREV = meta$stock_abbrev,
           SEX_TYPE = meta$sex_type,
           ASSESSMENT_ABBREV = meta$assessment_abbrev,
           SA_YEAR = meta$sa_year,
           YEAR = unique(blkdf$YEAR),
           LENGTH_UOM = 'CM',
           AGE_UOM = 'YEAR',
           SOURCE = '') %>%
    left_join(brSpecs, by = c('NESPP4', 'BLOCK_ID')) %>%
    relocate(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, YEAR, REGION_ID, BLOCK_ID,
             LENGTH_UOM, LENGTH, AGE_UOM, AGE, NO_AT_AGE, NESPP4,
             ASSESSMENT_ABBREV, SA_YEAR, SOURCE)
  
  return(out)
}