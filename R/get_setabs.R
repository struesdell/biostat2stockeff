
get_setabs <- function(specs, bsdir, yrsNoPR = NULL){
  
  # Get the file names for the input and output biostat files
  bsfps <- get_biostatPaths(pth = bsdir)
  
  # Build list of tabs for each file
  blk <- list()
  reg <- list()
  alkh <- list()
  lw <- list()
  yrs <- numeric(length(bsfps$ins))
  for(i in 1:length(bsfps$ins)){
    
    # Double-check that we are talking about the same input/output file
    if(!(basename2(bsfps$ins[i]) == basename2(bsfps$outs[i]))){
      stop('get_setabs: input/output files not the same')
    }
    
    bsd_tmp <- get_biostatData(inp = bsfps$ins[i], out = bsfps$outs[i])
    yrs[i] <- bsd_tmp$yr
    blk_tmp <- list()
    reg_tmp <- list()
    lw_tmp <- list()
    for(j in 1:length(specs$mkts)){
      if(specs$mkts[j] %in% bsd_tmp$mkts){
        blk_tmp[[j]] <- get_blocksTab_mkt(nespp4 = specs$mkts[j],
                                          bsdat = bsd_tmp,
                                          meta = specs)
        reg_tmp[[j]] <- get_regionsTab_mkt(nespp4 = specs$mkts[j],
                                           bsdat = bsd_tmp,
                                           meta = specs)
        lw_tmp_x <- get_lw_paramsTab_mkt(nespp4 = specs$mkts[j],
                                         bsdat = bsd_tmp,
                                         meta = specs,
                                         blocksTab_mkt = blk_tmp[[j]])
        lw_tmp[[j]] <- lw_tmp_x$out_tab
        blk_tmp[[j]] <- blk_tmp[[j]] %>%
          select(-LW_ID) %>%
          left_join(lw_tmp_x$out_lwid,
                    by = c('NESPP4', 'REGION_ID', 'BLOCK_ID'))
      }else{
        message(yrs[i], ': Market ', specs$mkts[j], ' not included')
      }
    }
    blk[[i]] <- blk_tmp %>% bind_rows()
    reg[[i]] <- reg_tmp %>% bind_rows()
    lw[[i]] <- lw_tmp %>% bind_rows()
    alkh[[i]] <- get_alk_holesTab(alkmoddf = bsd_tmp$alkmod,
                                  blkdf = blk[[i]],
                                  meta = specs)
  }
  
  lwdf <- lw %>%
    bind_rows()
  
  # LW_ID has to be numeric 1-99, so fix this up. Just get rid of length group
  # assignment at earlier stage.
  uid <- lwdf %>%
    select(ALPHA, BETA, LW_TYPE, LW_ID3) %>%
    distinct() %>%
    group_by(ALPHA, BETA, LW_TYPE) %>%
    mutate(LW_ID = cur_group_id()) %>%
    ungroup()
  
  lwdf <- lwdf %>%
    left_join(uid, by = join_by(ALPHA, BETA, LW_TYPE, LW_ID3)) %>%
    select(-LW_ID3) %>%
    arrange(LW_ID, LW_TYPE) %>%
    distinct()
  
  blkdf <- blk %>%
    bind_rows() %>%
    left_join(uid, by = join_by(LW_ID3)) %>%
    select(-LW_ID3, -LW_TYPE, -ALPHA, -BETA) %>%
    arrange(YEAR, REGION_ID, BLOCK_ID)
  regdf <- reg %>% bind_rows() %>% arrange(YEAR, REGION_ID, AREA)
  alkhdf <- alkh %>% bind_rows() %>% arrange(YEAR, REGION_ID, BLOCK_ID, LENGTH, AGE)
  
  prdf <- get_prorateTab(yrs, specs, yrsN = yrsNoPR) %>% arrange(YEAR)
  
  dname <- paste('SE_tabs', format(Sys.time(), "%F_%H-%M"), sep = '_')
  dir.create(dname)
  
  # In some cases there are BIOSTAT [AGEMOD] (i.e., stock efficiency tab
  # ALK_HOLES) that done make sense ... like in the BIOSTAT files they refer
  # to BLOCK_IDs that don't exist. Remove any of these and output them into
  # ALK_EXCLUDE.txt
  alkhdf %>%
    filter(is.na(REGION_ID)) %>%
    write_delim(file = file.path(dname, 'exclude_from_alk_holes.txt'))
  
  alkhdf <- alkhdf %>%
    filter(!is.na(REGION_ID))
  
  write_delim(blkdf, file = file.path(dname, 'BLOCKS.txt'))
  write_delim(regdf, file = file.path(dname, 'REGIONS.txt'))
  write_delim(lwdf, file = file.path(dname, 'LW_PARAMS.txt'))
  write_delim(alkhdf, file = file.path(dname, 'ALK_HOLES.txt'))
  write_delim(prdf, file = file.path(dname, 'PRORATE.txt'))
  
  # Warning if there appear to be missing years
  fullyrs <- min(yrs):max(yrs)
  if(length(yrs) != length(fullyrs)){
    missingYrs <- fullyrs[!fullyrs %in% yrs] %>%
      sort() %>%
      paste(., collapse = ', ')
    warning('Years not contiguous ... looks like you may be missing: ',
            missingYrs, '.')
  }
  
}






