### 1. SETUP for raffle input ---------------------------

## 1.1. Load libraries ---------------------------

lapply(c("sf","dplyr","spatstat","polyCub"), 
       library, character.only = TRUE)

## 1.2. Setup function, returns points and polys ---------------------------

raffle_setup <- function(points, polys) {
  
  points <-
    points %>%
    filter(Property_ID > 0) %>%
    arrange(Property_ID) %>%
    mutate(
      point_x = st_coordinates(.)[,1], # Get point coordinates for future use
      point_y = st_coordinates(.)[,2]
    )
  
  polys <-
    polys %>%
    filter(units > 0) %>%
    st_set_agr("constant") %>% # Prevent warnings from the st operations
    mutate(
      ID = as.character(ID), # Make sure polygon IDs not factors
      poly_area = st_area(.) # Calculate polygon areas
    ) %>% 
    st_set_agr("constant")
  
  return(list(points, polys))
}

### 2. RAFFLE FUNCTION, returns points  ---------------------------

raffle_function <- function(points, polys, distance = distance, diagnostic = diagnostic)  {
  
  # Generate buffers, intersect with polygons, estimate units, group by Property_ID
  intersects <-
    points %>%
    st_set_agr("constant") %>%
    st_buffer(dist = distance, nQuadSegs = 10) %>% 
    st_set_agr("constant") %>%
    st_intersection(polys) %>%
    mutate(int_units = as.numeric(units*st_area(.) / poly_area)) %>% 
    select(-units,-poly_area) %>% 
    st_set_agr("constant") %>%
    arrange(Property_ID, ID)
  
  # Transform intersects relative to point coordinates
  st_geometry(intersects) <-   
    mapply(function(geom,x,y){
      geom <- geom - c(x,y)
      geom
    },
    st_geometry(intersects),
    intersects$point_x,
    intersects$point_y,
    SIMPLIFY = FALSE) %>%
    st_sfc()
  
  # Integrate the PDF over intersect polygons
  intersects$probability <-
    mapply(function(geom,units){
      polyCub.midpoint(as(geom,"Spatial"), function(x) {
        dnorm(sqrt(x[,1]^2 + x[,2]^2), mean = 100, sd = 50, log = FALSE) *
          (1 / (2 * pi))}
        ) * 
        units
    },
    intersects$geometry,
    intersects$int_units)
  
  # Determine winners, add candidates field if diagnostic==TRUE
  results <-
    intersects %>%
    st_drop_geometry() %>%
    group_by(Property_ID)
  
  if (diagnostic == TRUE) {
    results <-
      results %>%
      summarize(winner = as.character(base::sample(ID, 1, prob = probability)),
                candidates = list(matrix(c(ID,(probability)/sum(probability)),ncol = 2))
      )
  } else {
    results <-
      results %>%
      summarize(winner = as.character(base::sample(ID, 1, prob = probability))
      )
  }
  
  points <-
    results %>% 
    left_join(points, ., by = "Property_ID") %>% 
    select(-point_x,-point_y)
  
  return(points)
}

### 3. MAIN compiler to run the whole process ---------------------------

raffle <- function(points, polys, distance = 200, diagnostic = FALSE, cores = 1){
  
  lapply(c("sf","dplyr","spatstat","polyCub"), 
         library, character.only = TRUE)
  
  setup <- raffle_setup(points, polys)
  points <- setup[[1]]
  polys <- setup[[2]]
  
  # Run single-core version by default, unless multiple cores are specified
  if (cores >= 2){
    library(parallel)
    points_list <- split(points, cut(seq_along(points$Property_ID),
                                     cores,
                                     labels = FALSE))
    points_list <- mclapply(points_list,
                            raffle_function,
                            polys = polys,
                            distance = distance,
                            diagnostic = diagnostic,
                            mc.cores = cores)
    points <- do.call(rbind, points_list)
    } else {
      points <- raffle_function(points,polys,distance = distance, diagnostic = diagnostic)
      }
  
  return(points)
}

points3 <- points[1:2000,]

time1 <- Sys.time()
raffled <- raffle(points, polys, cores = 6)
time2 <- Sys.time()
