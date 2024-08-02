get_lw_paramsTab_mkt <- function(nespp4, bsdat, meta, blocksTab_mkt){

  # Same strategy for naming regions as in get_regionsTab_mkt
  # Should just be sequential...
  mktID <- str_sub(nespp4, -1)
  
  if(nespp4 %in% bsdat$mkts){
  
    lw_mkt <- bsdat$lwblk %>%
      filter(MARKET == nespp4) %>%
      mutate(REGION_ID = paste0(REGION, mktID),
             LW_ID = paste(bsdat$yr, 1:n(), sep = '_'),
             LW_ID = paste(LW_ID, mktID, sep = '_'),
             SOURCE = 'x') %>%
      rename(BLOCK_ID = BLOCK) %>%
      left_join(blocksTab_mkt %>%
                  select(-LW_ID) %>%
                  mutate(BLOCK_ID = BLOCK_ID %>% as.character()),
                by = c('BLOCK_ID', 'REGION_ID')) %>%
      rename(LW_TYPE = BLOCK_TYPE)
    
    out_tab <- lw_mkt %>%
      select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, ASSESSMENT_ABBREV, SA_YEAR,
             ALPHA, BETA, SOURCE, LW_TYPE, LW_ID) %>%
      rename(LW_ID3 = LW_ID)
    
    out_lwid <- lw_mkt %>%
      select(NESPP4, REGION_ID, BLOCK_ID, LW_ID) %>%
      mutate(BLOCK_ID = as.integer(BLOCK_ID)) %>%
      rename(LW_ID3 = LW_ID)
    
    return(list(out_tab = out_tab, out_lwid = out_lwid))
    
  }else{
    message('get_lw_paramsTab: Market ', nespp4, ' not included')
  }
}

