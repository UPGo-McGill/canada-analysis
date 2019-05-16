## Temp cleanup

library(tidyverse)
library(sf)


load("data/Canada_daily.Rdata")
load("data/Canada_property.Rdata")



Canada_daily <- Canada_daily %>% 
  select(-ML, -day_num, -day_num_r, -lifelength, -FREH, -FREH2, -CMAgroup) %>% 
  rename("R365" = R3652, "A365" = A3652, "ML" = MLnew)

