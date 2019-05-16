##MAPPING

library(tmap)
library(ggmap)
library(sf)
library(cancensus)
library(tmaptools)
library(cowplot)


######### Data prep ###########

poly <- st_read("data", "DA")
poly <- st_transform(poly, 3347)
MTLpoly <- poly %>%
  filter(CMANAME == "Montréal")

Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C = c("01")), level = 'CT') #get CTs with population
cd <- cd %>% 
  select(GeoUID, Dwellings) %>% 
  rename(CTUID = GeoUID, units = Dwellings)

CT <- st_read("data", "gct_000b11a_e")
CT <- st_transform(CT, 3347)

library(foreign)
CT2 <- read.dbf("data/gct_000b11a_e.dbf", as.is = TRUE)
CT$CMANAME <- CT2$CMANAME
CT$CMANAME <- parse_character(CT$CMANAME, locale = locale(encoding = "Latin1"))

map <- Canada_FREH %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(CTUID) %>% 
  summarise(FREH = sum(FREH))

CT <- CT %>%
  left_join(cd, by = "CTUID") %>% 
  left_join(map, by = "CTUID") %>% 
  mutate(percFREH = FREH/units)




ggmap::register_google(key = "AIzaSyCNHR8TyrpbCB6p_tn0oyxKiV0FdtE0-Aw")

Canada_property <- Canada_property %>% 
  left_join(property[,c(1,19:20)], by = "Property_ID")


######## GGMAP BASICS ##########

myLocation <- "Montreal"
myMap <- get_map(location = myLocation, maptype = "roadmap", crop = FALSE, key = "AIzaSyCNHR8TyrpbCB6p_tn0oyxKiV0FdtE0-Aw", zoom = 12)
ggmap(myMap)+
  geom_density_2d(data = Canada_property, aes(x = Longitude, y = Latitude), size = 0)+
  stat_density2d(data = Canada_property, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.6), guide = FALSE)

ggplot() +
  geom_sf(data=MTLpoly, color = NA, aes(fill = CTUID), show.legend = FALSE)+
  coord_sf(crs = 4326, datum = NA)

ggplot() +
  geom_sf(data=MTLpoly, color = NA, aes(fill = CTUID), show.legend = FALSE)+
  coord_sf(crs = 4326, datum = NA, xlim = c(-73.4, -73.8), ylim = c(45.4,45.6))

########## TMAP BASICS ##########
tm_shape(MTLpoly, projection = 4326)+
  tm_fill("CTUID")+
  tm_legend(show = FALSE)  

tm_shape(MTLpoly, projection = 4326, xlim = c(-73.4, -73.5), ylim = c(45.4,45.5))+
  tm_fill("CTUID")+
  tm_legend(show = FALSE)


## facets tm  
tm_shape(CT)+
  tm_fill(col = "percFREH")+
  tm_facets(by = "CMANAME")

## bb to zoom
tm_shape(TO_CT, bbox = bb(TO_CT, .3), projection = 26918)+
  tm_fill(col = "CTUID")

########## POLYGON MAPS ##############
tm_shape(CT)+
  tm_fill(col = "percFREH")+
  tm_facets(by = "CMANAME") # how do I zoom per facet? iteration with zooming a constant amount with bb?



ggplot(CT_top) +
  geom_sf(color = NA, aes(fill = units), size = 0.4)+
  coord_sf(crs = 4326, datum = NA) +
  facet_wrap(~CMANAME)

CT_top <- CT %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver", "Calgary", "Ottawa-Gatineau", "Edmonton")) 
CT_top$percFREH <- na_if(CT_top$percFREH, 0)
ggplot(data = CT_top) +
  geom_sf(aes(fill = cut_interval(percFREH, length = .005)), color = NA, size = 0.05) +
  coord_sf(crs = 4326, xlim = c(-79.6, -79.2), ylim = c(43.4,43.8), datum = NA)+
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "purple", "orange", "pink"), na.value =  "grey10") + 
  theme(panel.background = element_rect(fill = "grey80", colour = "grey80"))

##Figure 9
coast <- st_read("data", "lhy_000h16a_e")
st_crs(coast) <- 3347
river <- st_read("data", "lhy_000d16a_e")
st_crs(river) <- 3347

CMA <- st_read("data", "lcma000a16a_e")
st_crs(CMA) <- 3347

CT <- st_read("data", "gct_000b11a_e")
CT <- st_transform(CT, 3347)

library(foreign)
CT2 <- read.dbf("data/gct_000b11a_e.dbf", as.is = TRUE)
CT$CMANAME <- CT2$CMANAME
CT$CMANAME <- parse_character(CT$CMANAME, locale = locale(encoding = "Latin1"))

map <- Canada_FREH %>% 
  filter(Date == "2018-12-31", Housing == TRUE, !is.na(CMATYPE)) %>% 
  group_by(CTUID) %>% 
  summarise(FREH = sum(FREH))

Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C = c("01")), level = 'CT') #get CTs with population
cd <- cd %>% 
  select(GeoUID, Dwellings) %>% 
  rename(CTUID = GeoUID)

CT <- CT %>%
  left_join(cd, by = "CTUID") %>% 
  left_join(map, by = "CTUID") %>% 
  mutate(percFREH = FREH/Dwellings)

#Toronto
library(tmap)
library(tmaptools)
breaks = c(0, 0.0005, 0.001, 0.0025, 0.005, .01, .025, .20)


TOCMA <- CMA %>%
  filter(CMANAME == "Toronto")

TOcoast <- coast %>% 
  st_intersection(TOCMA) %>% 
  sf::st_collection_extract("POLYGON")

TOriver <- river %>% 
  st_intersection(TOCMA) %>% 
  sf::st_collection_extract("LINE")

TO <- CT %>% 
  filter(CMANAME == "Toronto") %>% 
  mutate(percFREH = ifelse(Dwellings<25, NA, percFREH))

a <- tm_shape(TO, 
              projection = 4326, 
              xlim = c(-79.50, -79.27), 
              ylim = c(43.6, 43.76))+
  tm_fill(col = "percFREH", 
          colorNA = "grey90", 
          palette = "BuPu", 
          breaks = breaks)+
  tm_borders(alpha = 0)+
  tm_shape(TOcoast)+
  tm_fill(col = "grey30")+
  tm_shape(TOriver)+
  tm_lines(col = "grey30")+
  tm_layout(legend.show = FALSE, 
            bg.color = "grey30", 
            attr.color = "grey10")+
  tm_credits("Toronto", 
             position = c("LEFT", "TOP"), 
             size = 1.3, 
             col = "grey10")+
  tm_scale_bar(color.dark = "grey10", 
               color.light = "white")

#Montreal

MTLCMA <- CMA %>%
  filter(CMANAME == "Montréal")
MTLcoast <- coast %>% 
  st_intersection(MTLCMA) %>% 
  sf::st_collection_extract("POLYGON")
MTLriver <- river %>% 
  st_intersection(MTLCMA) %>% 
  sf::st_collection_extract("LINE")

MTL <- CT %>% 
  filter(CMANAME == "Montréal") %>% 
  mutate(percFREH = ifelse(Dwellings<25, NA, percFREH))
b <- tm_shape(MTL, 
              projection = 4326, 
              xlim = c(-73.65, -73.45), 
              ylim = c(45.45, 45.55))+
  tm_fill(col = "percFREH",
          colorNA = "grey90", 
          palette = "BuPu", 
          breaks = breaks)+
  tm_borders(alpha = 0)+
  tm_shape(MTLcoast)+
  tm_fill(col = "grey30")+
  tm_shape(MTLriver)+
  tm_lines(col = "grey30")+
  tm_layout(legend.show = FALSE, 
            bg.color = "grey50",
            attr.color = "grey10")+
  tm_credits("Montréal", 
             position = c("LEFT", "TOP"), 
             size = 1.3, 
             col = "grey10")+
  tm_scale_bar(color.dark = "grey10", 
               color.light = "white")

#Vancouver
VANCMA <- CMA %>%
  filter(CMANAME == "Vancouver")
VANcoast <- coast %>% 
  st_intersection(VANCMA) %>% 
  sf::st_collection_extract("POLYGON")
VANriver <- river %>% 
  st_intersection(VANCMA) %>% 
  sf::st_collection_extract("LINE")

VAN <- CT %>% 
  filter(CMANAME == "Vancouver") %>% 
  mutate(percFREH = ifelse(Dwellings<25, NA, percFREH))

c <- tm_shape(VAN, 
              projection = 4326, 
              xlim = c(-123.2, -122.99), 
              ylim = c(49.2, 49.4))+
  tm_fill(col = "percFREH", 
          breaks = breaks, 
          colorNA = "grey90", 
          palette = "BuPu")+
  tm_borders(alpha = 0)+
  tm_shape(VANcoast)+
  tm_fill(col = "grey30")+
  tm_shape(VANriver)+
  tm_lines(col = "grey30")+
  tm_compass(north = 0, 
             type = NA, 
             fontsize = 0.8, 
             size = NA,
             show.labels = 1, 
             cardinal.directions = c("N", "E", "S", "W"),
             text.color = NA, 
             color.dark = "grey10", 
             color.light = "white", 
             lwd = 1,
             position = c("right", "top"), 
             just = NA)+
  tm_layout(legend.show = FALSE, 
            bg.color = "grey50", 
            attr.color = "grey10")+
  tm_credits("Vancouver", 
             position = c("LEFT", "TOP"), 
             size = 1.3, 
             col = "grey10")+
  tm_scale_bar(color.dark = "grey10", 
               color.light = "white")


#Ottawa
OTTwat <- st_read("data/water-bodies.shp")

OTT <- CT %>% 
  filter(CMANAME == "Ottawa - Gatineau (Ontario part / partie de l'Ontario)" 
         | CMANAME == "Ottawa - Gatineau (partie du Québec / Quebec part)") %>% 
  mutate(percFREH = ifelse(Dwellings<25, NA, percFREH))

d <- tm_shape(OTT, 
         projection = 4326, 
         xlim = c(-75.8, -75.6), 
         ylim = c(45.37, 45.47))+
  tm_fill(col = "percFREH", 
          breaks = breaks, 
          colorNA = "grey90", 
          palette = "BuPu")+
  tm_borders(alpha = 0)+
  tm_shape(OTTwat)+
  tm_fill(col = "grey30")+
  tm_layout(legend.show = FALSE,
            bg.color = "grey50", 
            attr.color = "grey10")+
  tm_credits("Ottawa", 
             position = c("LEFT", "TOP"), 
             size = 1.3, 
             col = "grey10")+
  tm_scale_bar(color.dark = "grey10",
               color.light = "white")


#Calgary
CALCMA <- CMA %>%
  filter(CMANAME == "Calgary")

CALcoast <- coast %>% 
  st_intersection(CALCMA)

CALriver <- river %>% 
  st_intersection(CALCMA) %>% 
  sf::st_collection_extract("LINE")

CALwat <- st_read("data/geo_export_557ff889-50a4-46e1-b194-c26d6b2d6c7f.shp")

CAL <- CT %>% 
  filter(CMANAME == "Calgary") %>% 
  mutate(percFREH = ifelse(Dwellings<25, NA, percFREH))
e <- tm_shape(CAL, projection = 4326, xlim = c(-114.2, -114), ylim = c(50.95, 51.13))+
  tm_fill(col = "percFREH", breaks = breaks, colorNA = "grey90", palette = "BuPu")+
  tm_borders(alpha = 0)+
  tm_shape(CALwat)+
  tm_fill(col = "grey30")+
  tm_layout(legend.show = FALSE, 
            bg.color = "grey50",
            attr.color = "grey10")+
  tm_credits("Calgary", 
             position = c("LEFT", "TOP"), 
             size = 1.3, col = "grey10")+
  tm_scale_bar(color.dark = "grey10", 
               color.light = "white")

#Edmonton
EDCMA <- CMA %>%
  filter(CMANAME == "Edmonton")

EDwat <- st_read("data/CAN_water_areas_dcw.shp")
EDwat <- st_transform(EDwat, 3347)
EDwat <- EDwat %>% 
  st_intersection(EDCMA)

ED <- CT %>% 
  filter(CMANAME == "Edmonton") %>% 
  mutate(percFREH = ifelse(Dwellings<25, NA, percFREH))
f <- tm_shape(ED, projection = 4326, xlim = c(-113.7, -113.4), ylim = c(53.5, 53.6))+
  tm_fill(col = "percFREH", 
          breaks = breaks, 
          colorNA = "grey90", 
          palette = "BuPu",
          legend.is.portrait = FALSE,
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=2, format="f"), " %")))+
  tm_borders(alpha = 0)+
  tm_shape(EDwat)+
  tm_fill(col = "grey30")+
  tm_layout(legend.show = FALSE, 
            legend.width = 1, legend.height = .6,
            legend.just = "center",
            bg.color = "grey50", 
            attr.color = "grey10", 
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.bg.color = "white", 
            legend.text.color = "grey10", 
            legend.text.size = 1,
            legend.position = c("right", "BOTTOM"),
            legend.title.size = 0.0001)+
  tm_credits("Edmonton", 
             position = c("LEFT", "TOP"), 
             size = 1.3, col = "grey10")+
  tm_scale_bar(color.dark = "grey10", 
               color.light = "white")

tmap_arrange(a,b,c,d,e,f, ncol = 3)



########## POINT DENSITY ##############

ggplot() +
  geom_sf(data = TO_CT, fill = "grey20", colour = "grey10", size = 0.05) +
  coord_sf(crs = 4326, xlim = c(-79.8, -79.2), ylim = c(43.8,43.4), datum = NA)+
  geom_point(data = TO_prop, aes(x = Longitude, y = Latitude), size = 0.2, color = "grey")+
  stat_density2d(data = TO_prop, aes(x = Longitude, y = Latitude, fill = ..level..), 
                 alpha = 0.5, geom = "polygon", position = "identity")+
  theme_map()


crime_densities <- smooth_map(shp = TO_prop, cover.type = "original", 
                              buffer.width = 0.0000005, nlevels = 15, style = "kmeans")
crime_densities$polygons <- crime_densities$polygons[3:15,]
tm_shape(crime_densities$polygons)+tm_fill("level")

tm_shape(crime_densities$polygons) + tm_fill("level")
plot(crime_densities$polygons)

########### DOT DENSITY ###############
TO_CT <- CT %>% 
  filter(CMANAME == "Toronto")

TO_CT_union <- st_union(TO_CT)

TO_prop <- Canada_property %>% 
  filter(CMANAME == "Toronto")

ggplot() +
  geom_sf(data = TO_CT, fill = "transparent", colour = "white", size = 0.05) +
  geom_point(data = TO_prop, aes(Longitude, Latitude, colour = Listing_Type), 
             size = 0.000001, alpha = 0.8) +
  coord_sf(crs = 4326, datum = NA) +
  theme_void(base_family = "Iosevka", base_size = 12) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_void()+
  labs(x = NULL, y = NULL)+
  theme(plot.background = element_rect(fill = "#212121", color = NA), 
        panel.background = element_rect(fill = "#212121", color = NA),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", colour = NA),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        text =  element_text(color = "white"),
        title =  element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 32),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


















