## Temp cleanup

library(tidyverse)
library(sf)


load("data/Canada_daily.Rdata")
load("data/Canada_property.Rdata")



Canada_daily <- Canada_daily %>% 
  select(-ML, -day_num, -day_num_r, -lifelength, -FREH, -FREH2, -CMAgroup) %>% 
  rename("R365" = R3652, "A365" = A3652, "ML" = MLnew) %>% 
  filter(Date <= "2018-12-31")






## Notes about data files

# lda_000a16a_e is DA shapefile




Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
