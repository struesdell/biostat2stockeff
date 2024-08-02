

basename2 <- function(x){
  
  x %>%
    tools::file_path_sans_ext() %>%
    basename() %>%
    toupper()

}
