key <- 'pk.eyJ1IjoiamVubmk4MTQiLCJhIjoiY2p0a3MxYnF3MDFldTQzcGM1bXUzM3gwcyJ9.n05YO4LvUEHc5xFShK4shQ'    ## put your own token here
mapdeck(token = key)
set_token('pk.eyJ1IjoiamVubmk4MTQiLCJhIjoiY2p0a3MxYnF3MDFldTQzcGM1bXUzM3gwcyJ9.n05YO4LvUEHc5xFShK4shQ')


mapdeck(style = ms, pitch = 60) %>%
  add_grid(data = MTLprop2, layer_id = "grid_layer", elevation_scale = 20, cell_size = 200)

MTLprop2 <- Canada_property %>% 
  filter(CMANAME == "Montréal")


            