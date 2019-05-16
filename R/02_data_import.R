### DATA IMPORT

csd_units <- get_census("CA16", regions = list(C = c("01")), level = 'CSD') %>% 
  select(GeoUID, Dwellings, Population)
