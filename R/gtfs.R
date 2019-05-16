library(imputeTS)
library(ggplot2)
library(gganimate)
library(gtfsr)
library(sf)
library(tidytransit)
library(gganimate)

mtl <- read_gtfs("http://www.stm.info/sites/default/files/gtfs/gtfs_stm.zip", frequency = TRUE)


## temporary datafram of the train route ids

temp_df <- mtl$routes %>% 
  filter(route_type==1) %>% 
  select(route_id, route_long_name, route_color)


## Create data frames of the static geometries for lines and stops
## Aggregate individual points that make up a line, order them, convert to 
## sf line string

lines_df <- temp_df %>% 
  inner_join(mtl$trips, by = "route_id") %>% 
  distinct(route_id, shape_id) %>% 
  left_join(mtl$shapes, by = "shape_id") %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  left_join(mtl$trips, by = "shape_id") %>% 
  group_by(route_id) %>% 
  summarise() %>% 
  left_join(temp_df, by = "route_id") %>% 
  mutate(route_color = paste0("#", route_color))

## Then do the same thing for the individual stops

stops_df <- temp_df %>%
  inner_join(mtl$trips, by = "route_id") %>%
  left_join(mtl$stop_times, by = "trip_id")
  left_join(mtl$stops, by = "stop_id") %>%
  distinct(route_id, stop_id, route_color,
           stop_lon, stop_lat, stop_sequence) %>%
  mutate(route_color = paste0("#", route_color)) %>%
  rename(
    lat = stop_lat,
    lon = stop_lon
  )

# Getting the points of each track

shape_df <- temp_df %>%
  inner_join(mtl$trips, by = "route_id") %>%
  distinct(route_id, shape_id) %>%
  left_join(mtl$shapes, by = "shape_id") %>%
  group_by(shape_id) %>%
  rename(
    lat = shape_pt_lat,
    lon = shape_pt_lon
  ) %>%
  select(-shape_pt_sequence)

# Creating a dataframe of all individual trips
trips_df <- temp_df %>%
  full_join(mtl$trips, by = "route_id") %>%
  full_join(mtl$stop_times, by = "trip_id") %>%
  left_join(mtl$stops, by = "stop_id") %>%
  distinct(
    route_id, route_color, shape_id, trip_id,
    stop_lat, stop_lon, arrival_time, stop_sequence) %>%
  rename(
    lat = stop_lat,
    lon = stop_lon
  ) %>%
  mutate(time = as.POSIXct(
    arrival_time,
    format = "%H:%M:%S",tz = "UTC")) %>%
  na.omit()

temp_df_2 <- trips_df %>%
  group_by(trip_id, shape_id) %>%
  summarize() %>%
  left_join(shape_df, by = "shape_id")

trips_df <- trips_df %>%
  bind_rows(temp_df_2) %>%
  group_by(trip_id) %>%
  arrange(trip_id, stop_sequence)
  mutate(time = as.POSIXct(
    na.interpolation(
      as.numeric(time),
      option = "stine"),
    origin = '1970-01-01', tz = 'UTC')
  ) %>%
  fill(arrival_time) %>%
  select(trip_id, arrival_time, lat, lon, time, stop_sequence) %>%
  group_by(trip_id, stop_sequence) %>%
  filter(row_number() == 1)

library(cowplot)

ggplot() +
  geom_point(
    data = stops_df,
    aes(x = lon, y = lat, color = route_id),
    size = 2.5,
    show.legend = FALSE
  ) +
  geom_sf(
    data = lines_df,
    aes(color = route_id),
    show.legend = FALSE
  ) +
  geom_point(
    data = trips_df,
    aes(x = lon, y = lat),
    size = 1.5,
    shape = 15
  )+
  scale_color_manual(values = lines_df$route_color)+
  transition_components(time)+
  ease_aes("sine-in-out") +
  theme_bw() +
  theme(
    line = element_blank(),
    rect = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 24,
      margin = margin(b = -70, t = 42)),
    plot.subtitle = element_text(
      size = 18,
      margin = margin(b = -122, t = 80)),
    panel.grid.major = element_line(colour = "transparent")
  ) +
  theme_map()
  annotate(
    "text",
    x = -87.9350395,
    y = 41.734339,
    label = "Created by Dan Snow \ngithub.com/dfsnow",
    hjust = 0,
    size = 5,
    color = "grey60")


frames <- as.numeric(length(unique(trips_df$arrival_time))) * 3
plot_mg <- animate(plot = plot, nframes = 855, fps = 50)
