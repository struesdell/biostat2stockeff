
get_biostatData <- function(inp, out){
  
  inp <- readLines(inp)
  out <- readLines(out)
  
  # Year
  yr <- inp[str_which(inp, '\\[GENERAL\\]') + 1] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    magrittr::extract2(1) %>%
    magrittr::extract(1) %>%
    as.numeric()
  
  # number of market categories
  mk0_idx <- str_which(out, 'Market Category')[1]
  mk1_idx <- str_which(out, 'Time Period Specification')
  mkts <- out[(mk0_idx+2):(mk1_idx-2)] %>%
    str_squish() %>%
    substr(., start = 1, stop = 4)
  nmkt <- length(mkts)
  
  # Extract region and month blocks
  rm0_idx <- str_which(inp, '\\[MARKETS\\]')
  rm1_idx <- str_which(inp, '\\[LENWT\\]')
  rm <- inp[(rm0_idx+1):(rm1_idx-1)] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+'))
  
  rm_idx <- tibble(
    MKT = mkts
  ) %>%
    mutate(START = which(sapply(rm, '[', 1) %in% MKT),
           STOP = lead(START)-1,
           STOP = replace_na(STOP, length(rm)))
  
  
  # Extract age-length key modifications
  alkmod0_idx <- str_which(inp, '\\[AGEMOD\\]')
  alkmod1_idx <- str_which(inp, '\\[BOOTSTRAP\\]')
  
  # Seems like the program was updated to include bootstrapping, so account
  # for that.
  alkmod1_idx <- ifelse(length(alkmod1_idx) == 0, length(inp), alkmod1_idx)
  alkmod <- inp[(alkmod0_idx+2):(alkmod1_idx-1)] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    do.call(rbind, .) %>%
    as_tibble(.name_repair = 'unique') %>%
    suppressMessages() %>%
    rename_with(~c('NESPP4', 'BLOCK_ID', 'LENGTH', 'AGE', 'NO_AT_AGE')) %>%
    mutate(across(c(-NESPP4), as.integer))
  
  # Extract length-weight information
  lw0_idx <- str_which(inp, '\\[LENWT\\]')
  lw1_idx <- str_which(inp, '\\[AGEMOD\\]')
  lw <- inp[(lw0_idx+2):(lw1_idx-1)] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    do.call(rbind, .) %>%
    as_tibble(.name_repair = 'unique') %>%
    suppressMessages() %>%
    rename_with(~c('NESPP4', 'BLOCK_ID', 'ALPHA', 'BETA'))
  
  # Block specifications
  bs0_idx <- str_which(out, 'Blocks Regions Time Periods')
  bs1_idx <- str_which(out, 'Time Period Specification')-2
  bs <- out[(bs0_idx+2):(bs1_idx)] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    do.call(rbind, .) %>%
    as_tibble(.name_repair = 'unique') %>%
    suppressMessages() %>%
    rename_with(~c('NESPP4', 'MARKET_NAME',  'BLOCKS', 'REGIONS',
                   'TIME_PERIODS'))
  
  # Time period specification
  ts0_idx <- str_which(out, 'Time Period Specification')
  ts1_idx <- ts0_idx + 15
  tsnames <- out[ts0_idx+2] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    magrittr::extract2(1) %>%
    magrittr::extract(-1)
  tps <- out[(ts0_idx+4):(ts1_idx)] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    do.call(rbind, .) %>%
    as_tibble(.name_repair = 'unique') %>%
    suppressMessages() %>%
    rename_with(~c('MONTH', tsnames))
  
  lwblk0_idx <- str_which(out, 'Length Weight Coefficients Applied')
  lwblk1_idx <- str_which(out, 'Landings &  Samples') - 3
  lwblk <- out[(lwblk0_idx+4):lwblk1_idx] %>%
    str_squish() %>%
    lapply(., function(x) str_split_1(x, pattern = '\\s+')) %>%
    do.call(rbind, .) %>%
    as_tibble(.name_repair = 'unique') %>%
    suppressMessages() %>%
    rename_with(~c('MARKET', 'REGION', 'TIME_PERIOD', 'BLOCK', 'ALPHA', 'BETA'))
  
  return(list(yr = yr,
              mkts = mkts,
              nmkt = nmkt,
              rm = rm,
              rm_idx = rm_idx,
              lw = lw,
              alkmod = alkmod,
              bs = bs,
              tps = tps,
              lwblk = lwblk))
}
