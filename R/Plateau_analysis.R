## Daily Active Listings ----------------------------------------------------------

# line graph
active <- MTL_daily %>%
  filter(Housing == TRUE) %>% 
  group_by(Date) %>%
  summarise(n = n())
ggplot(active, aes(Date, n))+
  geom_line()

#y-o-y growth
active_growth <- MTL_daily %>%
  filter(Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>%
  group_by(Property_ID, year) %>% 
  summarise(n = n()) %>% 
  group_by(year) %>% 
  summarise(n = n())
growth_spread <- spread(active_growth, key = year, value = n)
growth_spread <- growth_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

# ltm revenue
ltm_revenue <- MTL_daily %>%
  filter(Date >= "2018-01-29" & Date <= "2019-01-28" & Housing == TRUE) %>% 
  summarise(sum(Price[Status == "R"]))

# rev growth
rev_growth <- MTL_daily %>%
  filter(Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>%
  group_by(Property_ID, year) %>% 
  summarise(rev = sum(Price[Status == "R"])) %>% 
  group_by(year) %>% 
  summarise(rev = sum(rev))
rev_growth_spread <- spread(rev_growth, key = year, value = rev)
rev_growth_spread <- rev_growth_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

## ENTIRE HOME LISTINGS ------------------------------------------------------------------------

# Percentage of active listings that are eh

eh_percent_2019 <- MTL_daily %>% 
  filter(Date == "2019-01-29" & Housing == TRUE) %>% 
  summarise(sum(Listng_Type == "Entire home/apt"/n()))

eh_percent_2018 <- MTL_daily %>% 
  filter(Date == "2018-01-29" & Housing == TRUE) %>% 
  summarise(sum(Listng_Type == "Entire home/apt"/n()))

eh_rev_growth <- MTL_daily %>%
  filter(Housing == TRUE & Status == "R") %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>%
  group_by(year) %>% 
  summarise(rev = sum(Price[Listing_Type == "Entire home/apt"])/sum(Price)) %>% 
eh_rev_growth_spread <- spread(eh_rev_growth, key = year, value = rev)
eh_rev_growth_spread <- eh_rev_growth_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

## MULTILISTINGS --------------------------------------------------------------------------------
# Percent of EH listings that are ML 

ML <- MTL_daily %>% 
  filter(Date == "2018-01-29" & Housing == TRUE & Listing_Type = "Entire home/apt") %>%
  summarise(ml = sum(ML == TRUE)/n())


# MAPS ------------------------------------------------------------------------------------------
#Point Density Map
library(tmap)
library(tmaptools)
library(sf)
library(cowplot)

#prep DA/CT files
CT <- st_read("data", "gct_000b11a_e")
CT$CMANAME <- as.character(CT$CMANAME)
CT$CMANAME <- parse_character(CT$CMANAME, locale = locale(encoding = "Latin1"))
CT$CSDNAME <- parse_character(CT$CSDNAME, locale = locale(encoding = "Latin1"))
CT <- st_transform(CT, 26918)
DA <- st_read("data", "DA")
DA <- st_transform(DA, 26918)
MTL_DA <- DA %>%
  filter(CMANAME == "Montréal")
MTL_CT <- CT %>%
  filter(CMANAME == "Montréal")


#CONVERT PROP FILE TO SF!!!
property_old <- read_csv("data/Canada_Property_2018-05-17.csv")
property_new <- read_csv("data/Canada_Property_2019-02-05.csv")

property_old <- property_old %>% 
  filter(!`Property ID` %in% property_new$`Property ID`)
property <- bind_rows(property_old,property_new)
rm(property_old,property_new)

property <- setDT(property)
property <- property[,c(1:7,22:24,26:29,41,45:47,50:51)]
names(property) <- c("Property_ID", "Host_ID", "Listing_Title", "Property_Type",
                     "Listing_Type", "Created", "Scraped", "Bedrooms", "Bathrooms",
                     "Max_Guests", "Response_Rate", "Response_Time", "Superhost",
                     "Cancellation_Policy", "Minimum_Stay", "Photos",
                     "Business_Ready", "Instant_Book", "Latitude", "Longitude")
property <- arrange(property, Property_ID)
property[,c(1:2,8:11,15:16)] <- lapply(property[,c(1:2,8:11,15:16)], as.integer)
property <- filter(property, Property_ID > 0, Host_ID > 0)
Canada_property <- Canada_property %>% 
  left_join(property[,c(1,19:20)], by = "Property_ID")
Canada_property <- st_as_sf(Canada_property, coords = c("Longitude", "Latitude"))
st_crs(Canada_property) <- 3347


#subset MTL points
MTL_prop <- Canada_property %>% 
  filter(CMANAME == "Montréal")
MTL_prop <- st_transform(MTL_prop, 3347)


#calculate densities
densities <- smooth_map(shp = MTL_prop, cover.type = "original", 
                              buffer.width = 0.0000005, nlevels = 15, style = "kmeans")
densities$polygons <- densities$polygons[3:15,]
densities$polygons <- st_transform(densities$polygons, 26918)

#plot map

tm_shape(MTL_CT)+
  tm_fill(col = "grey50")+
tm_shape(MTL_prop)+
  tm_symbols(shape = 21)
  
  tm_shape(densities$polygons)+
  tm_fill("level")

st_crs(densities$polygons)
st_crs(MTL_CT)

ggplot()+
  geom_sf(data = MTL_CT, fill = "grey50")+
  geom_sf(data = densities$polygons, aes(fill = level), color = NA)+
  theme_map()

