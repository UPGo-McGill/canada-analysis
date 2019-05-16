FREH_VFREH <- function(daily_prop){
  
  
  ##FREH Counts
  daily_prop$StatusNumR <- as.numeric(daily_prop$Status=="R") #turn R status into 1
  daily_prop$Revenue <- daily_prop$StatusNumR*daily_prop$Price #get listing's revenue
  daily_prop$StatusNumAR <- as.numeric(daily_prop$Status!="B") #turn A and R status into 1
  
  
  #calculate number of days in existence for EH listings
  FREH <- daily_prop %>% 
    filter(Listing_Ty == "Entire home/apt") %>%
    group_by(Property_I) %>% 
    mutate(lifelength = n())
  
  #remove any of the listings in existence for less than 365 days
  FREH <- FREH[FREH$lifelength>364,]
  
  FREH <- FREH[order(FREH$Property_I,FREH$Date),]
  
  #rolling sum for R count
  FREH <- FREH %>% 
    group_by(Property_I) %>% 
    mutate(Rcount = (runSum(StatusNumR,365)))
  
  
      

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











##Fastest correct way to get FREH/VFREH

#R count and AR count


FREH <- function(daily_prop, window = 365, FREH_Rcount = 60, FREH_ARcount = 120, VFREH_Rcount = 120, VFREH_ARcount = 240){

FREH <- daily_prop %>% 
  filter(Listing_Ty=="Entire home/apt") %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(test = cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) %>%
  mutate(test = cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), 365),0)) %>%
  mutate(FREH = ifelse(Rcount >= FREH_Rcount & ARcount >= FREH_ARcount, 1, 0))

FREH <- daily_prop %>%
  filter(Listing_Ty == "Entire home/apt") %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(FREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) >= 60 & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), 365),0)) >= 120, 1, 0)) %>% #FREH status
  mutate(VFREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) >= 120 & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), 365),0)) >= 140, 1, 0)) %>% #VFREH status
  mutate(FREHrev = ifelse(FREH == "1" & Status == "R", Price, 0)) %>% #revenue: is FREH and has a reservation that day
  mutate(FREHrevCum = ifelse(FREH == "1", cumsum(Status == "R" * Price), 0)) %>% #total revenue ever earned by an FREH listing
  mutate(FREHrevCum365 = ifelse(FREH == "1", (cumsum(Status=="R" * Price) - replace_na(lag(cumsum(Status=="R" * Price), 365),0))))


  
#assign a 1 or 0 based on VFREH/FREH status
FREH$FREH <- ifelse(FREH$Rcount >= FREH_Rcount & FREH$ARcount >= FREH_ARcount, 1, 0)
FREH$VFREH <- ifelse(FREH$Rcount >= VFREH_Rcount & FREH$ARcount >= VFREH_ARcount, 1, 0)

#FREH counts per city
FREH_tidy <- FREH %>%
  filter(FREH == "1") %>%
  group_by(Date, City) %>%
  summarise(n())

#VFREH counts per city
VFREH_tidy <- FREH %>%
  filter(VFREH=="1") %>%
  group_by(Date, City) %>%
  summarise(n())

list("FREH_tidy" = FREH_tidy, "VFREH_tidy" = VFREH_tidy)
}





#Revenue
FREH <- FREH %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(Revenue = Price * Status == "R") %>%
  mutate(FREHrev = ifelse(FREH == "1" & Status == "R", Price, 0)) %>% #is FREH and has a reservation that day
  mutate(FREHcumRev) = ifelse(FREH == "1", cumsum(Revenue), 0) %>% #total revenue ever earned by an FREH listing
  mutate(FREHcunRev365 = (cumsum(Status=="R") - lag(cumsum(Status=="R"), 365)))

  


  



##FREH Revenue: could be revenue earned on that day by an FREH listing
  # could be the total revenue earned by an FREH in the past 365 days
  # could be the total revenue ever earned by a listing which is today an FREH
  
  
FREH$FREHcumRev <- ifelse(FREH$FREH == "1", cumsum(FREH$Price), 0)



EHrev <- ifelse(FREH$FREH == "1" & FREH$Status == "R", FREH$Price, 0) 


FREH$VFREHrev <- ifelse(FREH$Rcount >= 120 & FREH$ARcount >= 240 & FREH$Status == "R", FREH$Price, 0)


#Revenue, cumulative all time
FREH$FREHcumRev <- ifelse(FREH$Rcount >= 60 & FREH$ARcount >= 120 & FREH$Status == "R", cumsum(as.numeric(FREH$Price)),0)
FREH$VFREHcumRev <- ifelse(FREH$Rcount >= 120 & FREH$ARcount >= 240 & FREH$Status == "R", cumsum(as.numeric(FREH$Price)), 0)






##Slower correct way to get FREH/VFREH

#Rcount and AR count
FREH <- daily_prop %>%
  filter(Listing_Ty == "Entire home/apt") %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(Rcount = rollapplyr(Status == "R", FUN = sum, width=365, align = "right", partial = TRUE)) %>%
  mutate(ARcount = rollapplyr(Status != "B", FUN = sum, width=365, align = "right", partial = TRUE)) 

#assign a 1 or 0 based on VFREH/FREH status
FREH$FREH <- ifelse(FREH$Rcount >= 60 & FREH$ARcount >= 120, 1, 0)
FREH$VFREH <- ifelse(FREH$Rcount >= 120 & FREH$ARcount >= 240, 1, 0)

#FREH counts per city
FREH_tidy2 <- FREH %>%
  filter(FREH == "1") %>%
  group_by(Date, City) %>%
  summarise(n())

#VFREH counts per city
VFREH_tidy <- FREH %>%
  filter(VFREH=="1") %>%
  group_by(Date, City) %>%
  summarise(n())



##Would be fastest best way, but can't get it to work becaus partial can never = True

FREH <- FREH %>% 
  group_by(Property_I) %>% 
  mutate(Rcount = roll_sum(StatusNumR, n=365, align = "right", partial = FALSE, fill = NA))
