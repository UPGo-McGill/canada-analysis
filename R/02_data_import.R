### DATA IMPORT

## Load private data files

load("data/Canada_daily.Rdata")
load("data/Canada_property.Rdata")

Canada_daily <- Canada_daily %>% 
  select(-ML, -day_num, -day_num_r, -lifelength, -FREH, -FREH2, -CMAgroup) %>% 
  rename("R365" = R3652, "A365" = A3652, "ML" = MLnew) %>% 
  filter(Date <= "2018-12-31")


# Load public data files

csd_units <- get_census("CA16", regions = list(C = c("01")), level = 'CSD') %>% 
  select(GeoUID, Dwellings, Population)
