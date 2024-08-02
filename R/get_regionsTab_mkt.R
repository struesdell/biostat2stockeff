## PROBLEM WITH REGIONS
## Because MT's code divides market by region but stockeff doesn't do that.
get_regionsTab_mkt <- function(nespp4, bsdat, meta){
  
  mktID <- str_sub(nespp4, -1)
  
  if(nespp4 %in% bsdat$mkts){
  
    mk_start <- bsdat$rm_idx %>%
      filter(MKT == nespp4) %>%
      pull(START)
    mk_stop <- bsdat$rm_idx %>%
      filter(MKT == nespp4) %>%
      pull(STOP)
    
    mrm <- bsdat$rm[mk_start:mk_stop]
    nreg <- mrm[[1]][2] %>% as.numeric()
    
    area_idx <- seq(3, nreg * 2 + 1, 2)
    areas <- mrm[area_idx]
    
    # Area name is cat region, marketID. Would be better to be cat mktID, 
    # region but worried about how "01", "02", etc might mess things up in
    # the data set. This way a non-zero integer is first.
    names(areas) <- paste0(1:length(areas), mktID)
    areas_df <- stack(areas) %>%
      as_tibble() %>%
      rename_with(~c('AREA', 'REGION_ID'))
    
    regSpec <- areas_df %>%
      relocate(REGION_ID, AREA) %>%
      mutate(YEAR = bsdat$yr) %>%
      mutate(SPECIES_ITIS = meta$species_itis,
             STOCK_ABBREV = meta$stock_abbrev,
             SEX_TYPE = meta$sex_type,
             ASSESSMENT_ABBREV = meta$assessment_abbrev,
             SA_YEAR = meta$sa_year,
             .before = 1) %>%
      arrange(YEAR, REGION_ID, AREA)
    
    return(regSpec)
  
  }else{
    message('Market ', nespp4, ' not included')
  }
  
}