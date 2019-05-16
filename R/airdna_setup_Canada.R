
daily_old <- read_csv("data/Canada_daily.csv")
daily_new <- read_csv("data/Canada_daily_new.csv")
prop_old <- read_csv("data/Canada_Property_2018-05-17.csv")
prop_new <- read_csv("data/Canada_Property_2019-02-05.csv")


daily_old <- daily_old %>% 
  filter(Date < "2018-05-01")
daily <- bind_rows(daily_old,daily_new)
rm(daily_old, daily_new)

prop_old <- prop_old %>% 
  filter(!`Property ID` %in% prop_new$`Property ID`)
property <- bind_rows(prop_old,prop_new)
rm(prop_old,prop_new)

## 1. FUNCTIONS

airdna_prep <- function(daily,property,crs=4326) {
  
  library(data.table)
  library(tidyverse)
  library(sf)

  # Daily file prep
  daily <- setDT(daily)
  daily <- daily[,c(1:3,5,8)]
  names(daily) <- c("Property_ID","Date","Status","Price","Reservation")
  daily[,c(1,4:5)] <- lapply(daily[,c(1,4:5)], as.integer)

  # Property file prep
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
  
  # Confirm there are the same number of listings in prop and daily files
  property <- filter(property, Property_ID %in% daily$Property_ID)
  daily <- filter(daily, Property_ID %in% property$Property_ID)

  # Create housing field
  housing_names <- c("House","Private room in house","Apartment","Cabin","Entire condominium",
                     "Townhouse","Condominium","Entire apartment","Private room","Loft",
                     "Place","Entire house","Villa","Guesthouse","Private room in apartment",
                     "Guest suite","Shared room in dorm","Chalet","Dorm","Entire chalet",
                     "Shared room in loft","Cottage","Resort","Serviced apartment","Other",
                     "Bungalow","Farm stay","Private room in villa","Entire loft",
                     "Entire villa","Private room in guesthouse","Island","Entire cabin",
                     "Vacation home","Entire bungalow","Earth house","Nature lodge","In-law",
                     "Entire guest suite","Shared room in apartment","Private room in loft",
                     "Tiny house","Castle","Earth House","Private room in condominium",
                     "Entire place","Shared room","Hut","Private room in guest suite",
                     "Private room in townhouse","Timeshare","Entire townhouse",
                     "Shared room in house","Entire guesthouse","Shared room in condominium",
                     "Cave","Private room in cabin","Dome house",
                     "Private room in vacation home","Private room in dorm",
                     "Entire serviced apartment","Private room in bungalow",
                     "Private room in serviced apartment","Entire Floor","Entire earth house",
                     "Entire castle","Shared room in chalet","Shared room in bungalow",
                     "Shared room in townhouse","Entire cottage","Private room in castle",
                     "Private room in chalet","Private room in nature lodge","Entire in-law",
                     "Shared room in guesthouse","Casa particular","Serviced flat","Minsu",
                     "Entire timeshare","Shared room in timeshare","Entire vacation home",
                     "Entire nature lodge","Entire island","Private room in in-law",
                     "Shared room in serviced apartment","Shared room in cabin","Entire dorm",
                     "Entire cave","Private room in timeshare","Shared room in guest suite",
                     "Private room in cave","Entire tiny house",
                     "Private room in casa particular (cuba)","Casa particular (cuba)",
                     "Private room in cottage","Private room in tiny house",
                     "Entire casa particular","")
  
  property <- mutate(property, Housing = ifelse(Property_Type %in% housing_names,TRUE,FALSE))
  rm(housing_names)
  
  # Merge daily and prop files
  daily <- daily %>%
    left_join(property[,c(1:2,5:7,21)], by = "Property_ID")

  
  # Remove out-of-range rows
  daily <- 
    daily %>% 
    as_tibble() %>% 
    filter(Date >= Created & Date <= Scraped)
  
  # Add columns
  daily <-
    daily %>% 
    arrange(Property_ID, Date) %>%
    group_by(Property_ID) %>%
    mutate(
      R365 = as.integer(cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)), # Number of R in last 365 days
      A365 = as.integer(cumsum(Status=="A") - replace_na(lag(cumsum(Status=="A"), 365),0)), # Number of A in last 365 days
      Revenue365 = as.integer(cumsum(as.numeric(Status=="R") * Price) - 
                                replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0))) # Total revenue in last 365 days
  
  # Add geometry and transform
  property <-
    property %>%
    as_tibble() %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs = crs)
  
  return(list(daily,property))
}
 

multilistings <- function(daily){
  
  library(dplyr)
  
  daily <-
    daily %>%
    group_by(Listing_Type, Host_ID, Date) %>%
    mutate(ML = ifelse(n()>=2 & Listing_Type=="Entire home/apt", TRUE,
                       ifelse(n()>=3 & Listing_Type=="Private room",TRUE, FALSE)))

  return(daily)
}


## 2. RAFFLE

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


## 3. SCRIPTS

library(tidyverse)

# Import files
Canada_daily <- read_csv("data/Canada_daily.csv")
Canada_property_new <- read_csv("data/Canada_property.csv")
Canada_polys <- read_sf("data","Canada_DA")

# Clean up
Canada_polys$units <- as.integer(Canada_polys$units)
names(Canada_polys) <- c("ID","units","geometry")

# Scripts
time1 <- Sys.time()
Canada_list <- airdna_prep(daily, property,3347)
Canada_property <- Canada_list[[2]]
Canada_daily <- multilistings(Canada_list[[1]])
rm(Canada_list)
time2 <- Sys.time()

Canada_points <- Canada_property[,c(1,20)]

time3<- Sys.time()
Canada_property <- raffle(Canada_points, Canada_polys, distance = 200, cores = 6)
time4 <- Sys.time()
