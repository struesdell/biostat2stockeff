# Function to get market category data frame
get_blocksTab_mkt <- function(nespp4, bsdat, meta){
  
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
    
    # Note that the blocks are not consistent across market categories in
    # BIOSTAT ... so I'm just creating my own blocking here.
    mon <- mrm[[length(mrm)]] %>%
      as_tibble() %>%
      rename_with(~'BLOCK_ID_MONTH') %>%
      mutate(MONTH = 1:12) %>%
      group_by(BLOCK_ID_MONTH) %>%
      summarize(MONTH_START = min(MONTH),
                MONTH_END = max(MONTH),
                BLOCK_TYPE = case_when(n() == 6 ~ 'SEMESTER',
                                       n() == 3 ~ 'QUARTER',
                                       n() == 12 ~ 'ANNUAL',
                                       TRUE ~ 'CUSTOM'),
                .groups = 'drop') %>%
      select(-BLOCK_ID_MONTH)
    
    mktregID <- paste0(1:nreg, mktID)
    mktBlkSpec <- expand_grid(REGION_ID = mktregID, mon) %>%
      mutate(BLOCK_ID = 1:n()) %>%
      relocate(REGION_ID, BLOCK_ID, BLOCK_TYPE, MONTH_START, MONTH_END) %>%
      mutate(SPECIES_ITIS = meta$species_itis,
             STOCK_ABBREV = meta$stock_abbrev,
             SEX_TYPE = meta$sex_type,
             ASSESSMENT_ABBREV = meta$assessment_abbrev,
             SA_YEAR = meta$sa_year,
             YEAR = bsdat$yr,
             NESPP4 = nespp4,
             .before = 1) %>%
      mutate(LW_ID = NA) %>%
      arrange(YEAR, REGION_ID, BLOCK_ID)
    
    return(mktBlkSpec)
    
  }else{
    message('get_blocksTab: Market ', nespp4, ' not included')
  }
}