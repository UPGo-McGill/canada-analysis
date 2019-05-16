##DAILY SCRIPTS

#Setup
setup <- function(property, daily) {
  
  library(tidyr)
  library(zoo)
  library(dplyr)
  library(tidyverse)
  library(TTR)
  daily_prop <- merge(daily, property, by = c("Property_I"), incomparables = NA) #merge daily and property files
  daily_prop #is this correct to have it return daily_prop
}

#Number of active listings per city - IS THIS OKAY, BECAUSE WE ALREADY SCRAPED IT TO MATCH THE LSD SO CAN'T I JUST COUNT ALL THE LISTINGS IN EXISTENCE?
active_listings <- function(daily_prop){
  
  r_tidy <- Canada_daily %>%
    filter(!is.na(CMATYPE)) %>% 
    group_by(Date, CMATYPE, CMANAME) %>%
    summarise(n())
  
  r_tidy2 <- r_tidy %>% 
    filter(Date <= "2019-01-28")
  
  Canada_daily_nojan <- Canada_daily %>%
    filter(Date<= "2019-01-28")
  
  r_tidy_month3 <- Canada_daily_nojan %>%
    filter(!is.na(CMATYPE)) %>% 
    mutate(month = format(as.Date(Date), "%Y-%m")) %>% 
    group_by(Date, month, CMATYPE, CMANAME) %>% 
    summarise(n()) %>% 
    group_by(month, CMATYPE, CMANAME) %>% 
    summarise(n = mean(`n()`))

    
write_csv(r_tidy2, "daily_listings.csv")
write_csv(r_tidy_month3, "monthly_listings.csv")




  avg_active_2018_PUMA_rev <-  daily %>%
    filter(Date >= "2017-09-01", Date <= "2018-08-31", Housing == TRUE) %>% 
    group_by(Date, name, PUMA) %>% 
    summarise(n(), sum(Price[Status == "R"])) %>% 
    group_by(name, PUMA) %>% 
    summarise(mean(`n()`))
  
  r_spread <- spread(data = r_tidy,
                     key = Date,
                     value = `n()`,
                     drop=FALSE)
  
  list("tidy" = r_tidy, "spread" = r_spread)
}


active_listings2 <- function(daily_prop){
  r_tidy <- daily_prop %>%
    group_by(Date, City) %>%
    summarise(n())
  
  r_tidy
}

# Revenue table per city
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


#Number of reservations per city
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


#FREH/VFREH counts/revenue

FREH_VFREH <- function(daily_prop){
  
  
  ##FREH Counts
  daily_prop$StatusNumR <- as.numeric(daily_prop$Status=="R") #turn R status into 1
  daily_prop$Revenue <- daily_prop$StatusNumR*daily_prop$Price #get listing's revenue
  daily_prop$StatusNumAR <- as.numeric(daily_prop$Status!="B") #turn A and R status into 1
  
  
  #calculate number of days in existence for EH listings
  FREH <- daily_prop %>% 
    filter(Listing_Ty == "Entire home/apt") %>%
    group_by(Property_I) %>% 
    mutate(lifelength = (length(Price)))
  
  #remove any of the listings in existence for less than 365 days
  FREH <- FREH[FREH$lifelength>364,]
  
  FREH <- FREH[order(FREH$Property_I,FREH$Date),]
  
  #rolling sum for R count
  FREH <- FREH %>% 
    group_by(Property_I) %>% 
    mutate(Rcount = (runSum(StatusNumR,365)))
  
  #rolling sum for AR count
  FREH <- FREH %>% 
    group_by(Property_I) %>% 
    mutate(ARcount = (runSum(StatusNumAR,365)))
  
  #assign a 1 or 0 based on VFREH/FREH status
  FREH$FREH <- ifelse(FREH$Rcount>59&FREH$ARcount>119, 1, 0)
  FREH$VFREH <- ifelse(FREH$Rcount>119&FREH$ARcount>239, 1, 0)
  
  #revenue sums that day only
  FREH$FREHrev <- ifelse(FREH$Rcount>59&FREH$ARcount>119, FREH$Revenue, 0)
  FREH$VFREHrev <- ifelse(FREH$Rcount>119&FREH$ARcount>239, FREH$Revenue, 0)

  
  #FREH counts per city
  FREH_tidy <- FREH %>%
    filter(FREH=="1") %>%
    group_by(Date, City) %>%
    summarise(n())
  
  FREH_spread <- spread(data = FREH_tidy,
                       key = Date,
                       value = `n()`,
                       drop=FALSE)
  
  #VFREH counts per city
  VFREH_tidy <- FREH %>%
    filter(VFREH=="1") %>%
    group_by(Date, City) %>%
    summarise(n())
  
  VFREH_spread <- spread(data = VFREH_tidy,
                        key = Date,
                        value = `n()`,
                        drop=FALSE)

  #FREH revenue per city
  FREHrev_tidy <- FREH %>%
    filter(FREH == "1") %>%
    group_by(Date, City) %>%
    summarise(sum(FREHrev))
  
  FREHrev_spread <- spread(data = FREHrev_tidy,
                       key = Date,
                       value = `sum(FREHrev)`,
                       drop=FALSE)
  
  #VFREH revenue per city
  VFREHrev_tidy <- FREH %>%
    filter(VFREH == "1") %>%
    group_by(Date, City) %>%
    summarise(sum(VFREHrev))
  
  VFREHrev_spread <- spread(data = VFREHrev_tidy,
                           key = Date,
                           value = `sum(VFREHrev)`,
                           drop=FALSE)
 
  list("FREH_tidy" = FREH_tidy, "FREH_spread" = FREH_spread, "VFREH_tidy" = VFREH_tidy, "VFREH_spread" = VFREH_spread, "FREHrev_tidy" = FREHrev_tidy, "FREHrev_spread" = FREHrev_spread, "VFREHrev_tidy" = VFREHrev_tidy, "VFREHrev_spread" = VFREHrev_spread)
}
  
  
##Multilistings - make sure to use the original file with all of the listings regardless of length of existence

ML <- function(daily_prop){
  
  ML1 <- daily_prop %>%
    filter(Listing_Ty == "Entire home/apt") %>%
    group_by(Host_ID, Date) %>%
    mutate(activeListings = n())
  
  ML_tidy <- ML1 %>%
    filter(activeListings >= 2) %>%
    group_by(Date, City) %>%
    summarise(n())
  
  ML_spread <- spread(data = ML_tidy,
                            key = Date,
                            value = `n()`,
                            drop=FALSE)
  
  list("ML_tidy" = ML_tidy, "ML_spread" = ML_spread)
}


#Checkers
sum(FREH$Date == "2015-09-30" & FREH$City == "Calgary" & FREH$Listing_Ty == "Entire home/apt" & FREH$VFREH == "1", na.rm = TRUE)

test <- FREH[FREH$Date== "2015-09-30" & FREH$City == "Calgary"& FREH$Listing_Ty == "Entire home/apt"& FREH$VFREH == "1",]
sum(test[,26], na.rm = TRUE)


