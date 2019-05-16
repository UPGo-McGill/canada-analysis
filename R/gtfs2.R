mtl_sf <- read_gtfs("http://www.stm.info/sites/default/files/gtfs/gtfs_stm.zip", frequency = TRUE, geometry = TRUE)


mtl_sf$stops_sf <- mtl_sf$stops_sf %>% 
  left_join(mtl_sf$stop_times[,c(4:6)])

summary <- mtl_sf$stops_sf %>% 
  group_by(stop_id) %>% 
  summarise(n = n())

ggplot()+
  geom_sf(data = summary, aes(color = n), size = .1)+
  scale_fill_manual(breaks = c(0, 100, 250, 500, 1000, 10000,50000,100000,200000))+
  theme_map()+
  coord_sf(datum = NA)
