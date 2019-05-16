library(rgdal)
library(cowplot)
library(downloader)
library(scatterpie)
library(RColorBrewer)

u <- "https://www.donneesquebec.ca/recherche/dataset/b57cdeb1-98e7-4db7-bb84-32530f0367eb/resource/95ab084b-727e-4322-9433-0fed7baa690d/download/artm-sm-od13.geojson"
downloader::download(url = u, destfile = "data/SM.JSON")

SM <- readOGR(dsn = "data/SM.JSON")

SM2 <- st_as_sf(SM)

ODmatrix <- read_csv("data/ODmatrix.csv")

SM2 <- SM2 %>% 
  left_join(ODmatrix, by = c("SM13" = "SM"))



coast <- st_read("data", "lhy_000h16a_e")
st_crs(coast) <- 3347
CMA <- st_read("data", "lcma000a16a_e")
st_crs(CMA) <- 3347
MTLCMA <- CMA %>%
  filter(CMANAME == "Montréal")
MTLcoast <- coast %>% 
  st_intersection(MTLCMA) %>% 
  sf::st_collection_extract("POLYGON")

SM4 <- SM2
SM4_prj <- st_transform(SM4, crs ="+proj=utm +zone=18 +ellps=WGS84
                        +datum=WGS84 +units=m +no_defs")
SM4_prj$centroid <- st_centroid(SM4_prj$geometry)

centroid <- as.data.frame(st_coordinates(SM4_prj$centroid))

SM3 <- SM4_prj
st_geometry(SM3) <- NULL
SM3 <- SM3 %>% 
  select(-RA, -centroid, -SM13_nom)
SM3$X <- centroid$X
SM3$Y <- centroid$Y
SM3 <- SM3 %>% 
  #mutate(radius = Origin + Destnation)+
  mutate(radius3 = ifelse(radius == 0, 0, log10(Origin + Destnation)*400))


ggplot()+
  geom_sf(data = SM4_prj, fill = NA, colour = "grey50", size = 0.05)+
  geom_sf(data = MTLcoast, fill = "lightblue",color = NA)+
  geom_scatterpie(aes(X, Y, r = radius3),
                  cols = c("Origin", "Destnation"),
                  color = NA,
                  data = SM3, alpha = 0.6)+
  theme_map()+
  coord_sf(datum = NA, 
           crs = 32618, 
           xlim = c(504907.44, 704907.44), 
           ylim = c(5000000, 5100000))+
  scale_fill_manual(values = mypalette, name = "", labels = c("Destination", "Origine"))+
  theme(legend.position = "none")
geom_scatterpie_legend(radius = SM3$radius3)


ggplot()+
  geom_sf(data = SM4_prj, fill = NA, colour = "grey50", size = 0.05)+
  geom_sf(data = MTLcoast, fill = "lightblue",color = NA)+
  geom_scatterpie(aes(X, Y, r = radius3),
                  cols = c("Origin", "Destnation"),
                  color = NA,
                  data = SM3, alpha = 0.6)+
  theme_map()+
  coord_sf(datum = NA, 
           crs = 32618, 
           xlim = c(594907.44, 614907.44), 
           ylim = c(5030000, 5050000))+
  scale_fill_manual(values = mypalette, name = "", labels = c("Destination", "Origine"))+
  theme(legend.position = "none")
geom_scatterpie_legend(radius = SM3$radius3)


mypalette <- c("navy", "purple")


  
  




ggplot() +
  geom_sf(data = TO_CT, fill = "grey20", colour = "grey10", size = 0.05) +
  coord_sf(crs = 4326, xlim = c(-79.8, -79.2), ylim = c(43.8,43.4), datum = NA)+
  geom_point(data = TO_prop, aes(x = Longitude, y = Latitude), size = 0.2, color = "grey")+
  stat_density2d(data = TO_prop, aes(x = Longitude, y = Latitude, fill = ..level..), 
                 alpha = 0.5, geom = "polygon", position = "identity")+
  theme_map()
