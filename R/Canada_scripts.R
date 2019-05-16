## DAILY SCRIPTS

# Setup
setup <- function(property, daily) {
  
  library(tidyr)
  library(zoo)
  library(dplyr)
  library(tidyverse)
  library(TTR)
  daily_prop <- merge(daily, property, by = c("Property_I"), incomparables = NA) # merge daily and property files
  daily_prop <- daily_prop %>% 
    arrange(Property_I, Date) %>%
    group_by(Property_I) %>%
    mutate(day_num = row_number()) %>% # days since created
    mutate(day_num_r = ifelse(row_number() - match("R", Status) >= 0, row_number() - match("R", Status) + 1, NA)) %>% # days since first reservation
    mutate(R365 = cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) %>% # Number of R in last 365 days
    mutate(A365 = cumsum(Status=="A") - replace_na(lag(cumsum(Status=="A"), 365),0)) %>% # Number of A in last 365 days
    mutate(revenue365 = cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0)) # Total revenue in last 365 days
  
  
  daily_prop # is this correct to have it return daily_prop?
}

# Number of active listings
active_listings <- function(daily_prop){
  r_tidy <- daily_prop %>%
    group_by(Date, City) %>%
    summarise(n())
  
  r_spread <- spread(data = r_tidy,
                     key = Date,
                     value = `n()`,
                     drop=FALSE)
  
  list("tidy" = r_tidy, "spread" = r_spread)
}


# Total Revenue
active_revenue <- function(daily_prop) {
  
  rev_tidy <- daily_prop %>%
    filter(Status == "R") %>%
    group_by(Date, City) %>%
    summarise(sum(Price))
  
  rev_spread <- spread(data = rev_tidy,
                       key = Date,
                       value = `sum(Price)`,
                       drop=FALSE)
  
  list("tidy" = rev_tidy, "spread" = rev_spread)
}


# Number of reservations
reservations <- function(daily_prop){
  
  res_tidy <- daily_prop %>%
    filter(Status=="R") %>%
    group_by(Date, City) %>%
    summarise(n())
  
  res_spread <- spread(data = res_tidy,
                       key = Date,
                       value = `n()`,
                       drop=FALSE)
  
  list("tidy" = res_tidy, "spread" = res_spread)
}

# FREH/VFREH counts/revenue

FREH_VFREH <- function(daily_prop, window = 365, FREH_R = 60, FREH_AR = 120, VFREH_R = 120, VFREH_AR = 240) {
    
    # Add A/R fields
  daily_prop <- daily_prop %>%
    arrange(Property_I, Date) %>%
    group_by(Property_I) %>%
    mutate(R365 = cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) %>%
    mutate(A365 = cumsum(Status=="A") - replace_na(lag(cumsum(Status=="A"), 365),0)) %>%
    mutate(revenue365 = cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0))
           
  FREH <- daily_prop %>%
      filter(Listing_Ty == "Entire home/apt") %>%
      arrange(Property_I, Date) %>%
      group_by(Property_I) %>%
      mutate(FREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), window),0)) >= FREH_R & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), window),0)) >= FREH_AR, 1L, 0L)) %>% #FREH status
      mutate(VFREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) >= VFREH_R & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), window),0)) >= VFREH_AR, 1L, 0L)) %>%#VFREH status
      mutate(FREHrev = ifelse(FREH == "1" & Status == "R", Price, 0)) %>% #revenue: is FREH and has a reservation that day
      mutate(FREHrevCum = ifelse(FREH == "1", cumsum(as.numeric(Status == "R") * Price), 0)) %>% #total revenue ever earned by an FREH listing
      mutate(FREHrevCum365 = ifelse(FREH == "1", cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), window),0),0)) %>% #total revenue earned by a FREH listing in the last 365 days
      mutate(VFREHrev = ifelse(VFREH == "1" & Status == "R", Price, 0)) %>% #revenue: is VFREH and has a reservation that day
      mutate(VFREHrevCum = ifelse(VFREH == "1", cumsum(as.numeric(Status == "R") * Price), 0)) %>% #total revenue ever earned by an VFREH listing
      mutate(VFREHrevCum365 = ifelse(VFREH == "1", cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), window),0),0)) #total revenue earned by a VFREH listing in the last 365 days
  
    FREH_tidy <- FREH %>%
      filter(FREH == "1") %>%
      group_by(Date, City) %>%
      summarise(n(), sum(FREHrev), sum(FREHrevCum), sum(FREHrevCum365))
  
    VFREH_tidy <- FREH %>%
      filter(VFREH == "1") %>%
      group_by(Date, City) %>%
      summarise(n(), sum(VFREHrev), sum(VFREHrevCum), sum(VFREHrevCum365))
    
    list("FREH_tidy" = FREH_tidy, "VFREH_tidy" = VFREH_tidy)
}

## Multilistings

ML <- function(daily_prop){
  
  # add a column with number of active listings a host has 
  ML_EH <- daily_prop %>%
    filter(Listing_Ty == "Entire home/apt") %>%
    group_by(Host_ID, Date) %>%
    mutate(ML = ifelse(n()>=2, 1L, 0L))
  
  ML_PR <- daily_prop %>%
    filter(Listing_Ty == "Entire home/apt") %>%
    group_by(Host_ID, Date) %>%
    mutate(ML = ifelse(n()>=3, 1L, 0L))
  
  daily_prop4$ML <- NA
  daily_prop4[daily_prop4$Listing_Ty == "Entire home/apt"]$ML <- ML_EH$ML
  daily_prop4[daily_prop4$Listing_Ty == "Private room"]$ML <- ML_PR$ML
  
    ungroup()%>%
    group_by(Property_I) %>%
    mutate(MLrev = ifelse(ML == "1" & Status == "R", Price, 0)) %>%
    mutate(MLrevCum365 = ifelse(ML == "1", cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0),0)) %>% #total revenue earned by a FREH listing in the last 365 days 
    mutate(MLrevCum = ifelse(ML == "1", cumsum(as.numeric(Status == "R") * Price), 0)) #total revenue ever earned by an FREH listing
  
  ML_tidy <- ML %>%
    filter(ML == "1") %>%
    group_by(Date, City) %>%
    summarise(n())
  
  ML_spread <- spread(data = ML_tidy,
                      key = Date,
                      value = `n()`,
                      drop=FALSE)
  
  list("tidy" = ML_tidy, "spread" = ML_spread) # not currently creating tidy/spread revenue tables, can add easily though
}

