

library(tidyverse)
list.files('r', full.names = TRUE) %>%
  lapply(., source)

info <- list(species_itis = '169182',
              stock_abbrev = 'UNIT',
              sex_type = 'NONE',
              assessment_abbrev = 'MT',
              sa_year = '2021',
              mkts = c('3290', '3292', '3295', '3296'))

get_setabs(specs = info, bsdir = 'biostat')






