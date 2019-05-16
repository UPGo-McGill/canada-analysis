## NYC February update

# Import files

NYC_daily_2018 <- read_csv("data/NYC_daily_2018.csv")
NYC_daily_2017 <- read_csv("data/NYC_daily_2017.csv")
NYC_daily <- bind_rows(NYC_daily_2017,NYC_daily_2018)
rm(NYC_daily_2017, NYC_daily_2018)

NYC_property_2018 <- read_csv("data/NYC_property_2018.csv")
NYC_property_2017 <- read_csv("data/NYC_property_2017.csv")
NYC_property_2017 <- NYC_property_2017 %>% 
  filter(!`Property ID` %in% NYC_property_2018$`Property ID`)
NYC_property <- bind_rows(NYC_property_2017,NYC_property_2018)
rm(NYC_property_2017,NYC_property_2018)

NYC_polys <- read_sf("data", "NY_bg")
NYC_polys$units <- as.integer(NYC_polys$units)
names(NYC_polys) <- c("ID","units","geometry")
NYC_polys <- st_transform(NYC_polys,32618)

NYC_list <- airdna_prep(NYC_daily,NYC_property, NYC_polys, crs = 32618)
NYC_property <- NYC_list[[2]]
NYC_daily <- NYC_list[[1]]
rm(NYC_list)













Canada_property <- Canada_list[[2]]
Canada_daily <- multilistings(Canada_list[[1]])
rm(Canada_list)

Canada_property <- raffle(Canada_property, Canada_polys, distance = 200, segs = 10, max = 2500, cores = 6)




