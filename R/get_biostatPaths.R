
get_biostatPaths <- function(pth){
  
  # Read biostat input and output files (both are needed)
  inputs <- list.files(pth, full.names = TRUE,
                       pattern = '.INP', ignore.case = TRUE)
  
  outputs <- list.files(pth, full.names = TRUE,
                        pattern = '.out', ignore.case = TRUE)
  
  # Double-check that there are both input and output files for each
  # base name
  in_base <- basename2(inputs)
  
  out_base <- basename2(outputs)
  
  # Stop if there aren't exactly the same .in and .out files
  if(!setequal(in_base, out_base)){
    
    tibble(.IN = inputs, mch = in_base) %>%
      full_join(tibble(.OUT = outputs, mch = out_base), by = 'mch') %>%
      select(-mch) %>%
      print(n = 1000)
    
    stop('get_biostatPaths: input and output file names don\'t seem to all',
         'match ... does every biostat run have a .inp and .out in the',
         'directory?')
  }
  
  return(list(ins = inputs, outs = outputs))
  
}



