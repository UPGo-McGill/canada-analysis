## 1. FUNCTIONS

airdna_prep <- function(daily,property,crs=4326) {
  
  library(tidyverse)
  library(sf)

  # Daily file prep
  daily <- as_tibble(daily)
  daily <- daily[,c(1:3,5,8)]
  names(daily) <- c("Property_ID","Date","Status","Price","Reservation")
  daily[,c(1,4:5)] <- lapply(daily[,c(1,4:5)], as.integer)

  # Property file prep
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
  
  # Merge daily and prop files
  daily <- merge(daily, property[,c(1:2,5:7,21)], by = "Property_ID", incomparables = NA)
  
  daily <- daily %>% 
    left_join(property, by = "Property_ID")
  
  # Remove out-of-range rows
  daily <- filter(daily, Date >= Created & Date <= Scraped)
  
  # Add columns
  daily <-
    daily %>% 
    arrange(Property_ID, Date) %>%
    group_by(Property_ID) %>%
    mutate(R365 = as.integer(cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0))) %>% # Number of R in last 365 days
    mutate(A365 = as.integer(cumsum(Status=="A") - replace_na(lag(cumsum(Status=="A"), 365),0))) %>% # Number of A in last 365 days
    mutate(Revenue365 = as.integer(cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0))) # Total revenue in last 365 days
  
  # Add geometry and transform
  property <-
    property %>%
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
lapply(c("sf","SimplicialCubature","sfdct", "dplyr","parallel"), 
       library, character.only = TRUE)

## 1.2. Setup function, returns points and polys ---------------------------
raffle_setup <- function(points, polys) {
  points <-
    points %>%
    filter(Property_ID > 0) %>%
    arrange(Property_ID)
  
  points$cent_x <- st_coordinates(points)[,1] # Get point coordinates for future use
  points$cent_y <- st_coordinates(points)[,2] 
  points$winner <- as.character(NA) # Supply default winner value of NA
  
  polys$ID <- as.numeric(as.character(polys$ID))  # Make sure polygon IDs not factors
  polys <- polys %>% filter(units > 0)  # Discard polygons with 0 housing units
  st_agr(polys) = "constant"  # Prevent warnings from the st operations
  polys$poly_area <- st_area(polys)  # Calculate the polygon areas
  
  return(list(points, polys))
}

## 1.3. Coordinate function for matrix integration  ---------------------------
coordinate <- function(x) { 
  coord <- array(dim = c(2, 3, length(x$Property_ID)))
  # Get triangle vertex coordinates relative to buffer centroid
  coord[1,1,] <- st_coordinates(x)[c(TRUE,rep(FALSE,3)),1] - x$cent_x 
  coord[1,2,] <- st_coordinates(x)[c(FALSE,TRUE,rep(FALSE,2)),1] - x$cent_x
  coord[1,3,] <- st_coordinates(x)[c(rep(FALSE,2),TRUE,FALSE),1] - x$cent_x
  coord[2,1,] <- st_coordinates(x)[c(TRUE,rep(FALSE,3)),2] - x$cent_y
  coord[2,2,] <- st_coordinates(x)[c(FALSE,TRUE,rep(FALSE,2)),2] - x$cent_y
  coord[2,3,] <- st_coordinates(x)[c(rep(FALSE,2),TRUE,FALSE),2] - x$cent_y
  return(coord)
}

## 1.4 Probability distribution function ---------------------------
pdf_x <- function(x) (1 / sqrt(5000 * pi)) * 1 / 
  (2 * pi * sqrt(x[1]^2 + x[2]^2)) * exp(-(sqrt(x[1]^2 + x[2]^2) - 100)^2 / 5000)

### 2. RAFFLE FUNCTION CONSTITUENTS  ---------------------------
## 2.1. Create geometries  ---------------------------
# Apply the function to points_list, clean up, and pass the output
create_geometries <- function(points, polys, distance = distance, segs = segs)  {
  # Generate buffers around points
  buffers <- st_buffer(points, dist = distance, nQuadSegs = segs) 
  st_agr(buffers) = "constant"
  st_agr(polys) = "constant"
  intersects_all <- st_intersection(buffers, polys)  # Intersect buffers with polygons
  intersects_all <- intersects_all %>% arrange(Property_ID, ID)
  # Estimate the number of units in each intersect polygon
  intersects_all$int_units <- 
    as.numeric(intersects_all$units*st_area(intersects_all) / intersects_all$poly_area)
  st_agr(intersects_all) = "constant"
  # Divide intersect polygons by Property_ID
  int_list_all <- split(intersects_all, intersects_all$Property_ID) 
  # For points with only one intersect, declare winner
  points[points$Property_ID %in% 
           names(int_list_all[sapply(int_list_all, function(x) length(x$Property_ID)) == 1]),]$winner <- 
    sapply(int_list_all[sapply(int_list_all, function(x) length(x$Property_ID)) == 1], function(x) x$ID) 
  # Reduce points to ones with intersects > 1, to avoid unnecessary integration
  points_subset <- points[points$Property_ID %in% names(int_list_all[sapply(int_list_all,
                                                                            function(x) length(x$Property_ID)) > 1]),] 
  # Reduce intersect polygons correspondingly
  intersects <- intersects_all[intersects_all$Property_ID %in% points_subset$Property_ID,] 
  # Re-divide intersect polygons by Property_ID
  int_list <- int_list_all[names(int_list_all) %in% points_subset$Property_ID] 
  # Produce triangles out of intersect polygons and decompose GEOMETRYCOLLECTION objects into POLYGON objects
  triangles <- st_cast(ct_triangulate(intersects, trim = FALSE)) 
  return(list(points, intersects, triangles,points_subset))
}

## 2.2. Integration of the Gaussian PDF per triangle ---------------------------
pdf_integration <- function(points, intersects, triangles, points_subset, max = max) {
  # The univariate PDF in cartesian coordinates, with x[1] and x[2]
  # Convert triangles sf object into list, by intersect polygon
  tri_list <- split(triangles,paste(as.character(triangles$Property_ID),
                                    as.character(triangles$ID),sep=".")) 
  coordinate_list <- lapply(tri_list, coordinate) # Create coordinate matrix
  # Integrate the PDF over each intersect polygon, and weight by int_units
  intersects$prob <- sapply(coordinate_list,
                            function(x) adaptIntegrateSimplex(pdf_x, x, maxEvals = max)[[1]]) * intersects$int_units
  ## Rerun the integration for any points with NA values
  # Run the condition as long as there are any NAs or negative probabilities
  while (sum(is.na(intersects$prob)) > 0 | sum(intersects$prob < 0, na.rm = TRUE) > 0) { 
    max <- 2 * max # Increase the maxEvals of the integration
    # Create vector of Property_IDs to rerun integration with higher maxEvals
    prop_to_replace <- unique(intersects[is.na(intersects$prob) | intersects$prob<0, ]$Property_ID) 
    # Redo integration for all triangles with a compromised Property_ID
    tri_list2 <- unlist(lapply(split(triangles[triangles$Property_ID %in% prop_to_replace,], 
                                     triangles[triangles$Property_ID %in% prop_to_replace,]$Property_ID),
                               function(x) split(x, x$ID)), recursive = FALSE)
    coordinate_list2 <- lapply(tri_list2, coordinate) # Create coordinate matrix
    intersects[intersects$Property_ID %in% prop_to_replace, ]$prob <- 
      sapply(coordinate_list2, function(x) adaptIntegrateSimplex(pdf_x, x, maxEvals = max)[[1]]) * 
      intersects[intersects$Property_ID %in% prop_to_replace,]$int_units
  }
  ## Choose the winner
  # For points with multiple intersects, choose randomly
  points[points$Property_ID %in% points_subset$Property_ID,]$winner <- 
    sapply(split(intersects, intersects$Property_ID), 
           function(x) sample(x$ID, 1, prob = x$prob))
  points$winner
}

## 2.3. Raffle function to bring it all together ---------------------------
raffle_function <- function(points, polys, distance = distance, segs = segs, max = max) {
  setup <- raffle_setup(points, polys)
  points <- setup[[1]]
  polys <- setup[[2]]
  
  geometries <- create_geometries(points, polys, distance = distance, segs = segs)
  points <- geometries[[1]]
  intersects <- geometries[[2]]
  triangles <- geometries[[3]]
  points_subset <- geometries[[4]]
  winner <- pdf_integration(points, intersects, triangles, points_subset, max = max)
  return(winner)
}

### 3. MAIN compiler to run the whole process ---------------------------
## 3.1. Main raffle function ---------------------------
raffle <- function(points, polys, distance = 200, segs = 10, max = 2500, cores = 1){
  points_list <- split(points, cut(seq_along(points$Property_ID), max(c(cores,2)), labels = FALSE))
  winners <- mclapply(points_list, raffle_function, polys=polys, distance = distance, segs = segs, max = max, mc.cores = cores)
  points$winner <- unlist(winners)
  return(points)
}


## 3. SCRIPTS

time1 <- Sys.time()

library(tidyverse)

# Import files
daily <- read_csv("data/Canada_daily.csv")
property <- read_csv("data/Canada_prop2.csv")
polys <- read_sf("data","Canada_DA")

# Clean up
polys$units <- as.integer(polys$units)
names(polys) <- c("ID","units","geometry")

# Scripts
Canada_list <- airdna_prep(daily,property,3347)
property <- Canada_list[[2]]
daily <- multilistings(Canada_list[[1]])
rm(Canada_list)
time2 <- Sys.time()
points <- property %>% 
  select(Property_ID, geometry)

#ignore old raffle script

Canada_property <- raffle(Canada_property, Canada_polys, distance = 200, segs = 10, max = 2500, cores = 3)


