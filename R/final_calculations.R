
##Prep files-----------------------------------------------------------------------------
Canada_daily <- Canada_daily %>% 
  ungroup()
Canada_FREH <- Canada_FREH %>% 
  ungroup()
Candaa_daily_red4 <- Canada_daily %>% 
  ungroup() %>% 
  filter(Date>="2018-01-01",
         Date<="2018-12-31",
         Housing == TRUE,
         !is.na(CMATYPE))
Candaa_daily_red4 <- Candaa_daily_red4 %>% 
  ungroup()
Candaa_daily_red3 <- Canada_daily %>% 
  ungroup() %>% 
  filter(Date>="2017-01-01",
         Date<="2017-12-31",
         Housing == TRUE,
         !is.na(CMATYPE))
Candaa_daily_red3 <- Candaa_daily_red3 %>% 
  ungroup()
Canada_daily_all <- Canada_daily %>% 
  ungroup() %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Date <="2018-12-31", Date >= "2016-09-01")
Candaa_daily_all <- Candaa_daily_all %>% 
  ungroup()
Canada_daily_seas <- Canada_daily %>% 
  ungroup() %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Date <="2018-12-31", Date >= "2017-01-01")

## number of housing units removed from analysis
sum(Canada_property$Housing == FALSE & Canada_property$Created <= "2018-12-31" & Canada_property$Scraped >= "2016-09-01")

## revenue from housing removed from analysis
test <- Canada_daily %>% 
  filter(Date <="2018-12-31", Date >= "2018-01-01", Status == "R")
sum(test$Price[test$Housing == FALSE])/sum(test$Price) 

## unique listings with at least one booking 
Canada_daily_all %>% 
  ungroup()
  group_by(Property_ID) %>% 
  summarise(n = n())

## unique listings with at least one booking ltm
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Property_ID) %>% 
  summarise(n = n())

##active listings overall (Table 1) ----------------------------------------------------------------

#ADL overall
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Property_ID, Date) %>% 
  summarise(n = n()) %>% 
  group_by(Date) %>% 
  summarise(n = n()) %>% 
  summarise(mean(n))

#ADL in each CMATYPE
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(CMATYPE, Date) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>%
  summarise(mean(n))

#ADL in M, T, V
Candaa_daily_red4 %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(Date) %>% 
  summarise(n = n()) %>% 
  summarise(mean(n))

#growth in ADL by CMATYPE
Canada_daily_all %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>% 
  group_by(Date, year, CMATYPE) %>% 
  summarise(n = n()) %>% 
  group_by(year, CMATYPE) %>% 
  summarise(mean = mean(n)) %>% 
  spread(key = year, value = mean) %>% 
  mutate(growth = (LTM-MTM)/MTM)

#growth in ADL in M, T, V
Canada_daily_all %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>% 
  group_by(Date, year) %>% 
  summarise(n = n()) %>% 
  group_by(year) %>% 
  summarise(mean = mean(n)) %>% 
  spread(key = year, value = mean) %>% 
  mutate(growth = (LTM-MTM)/MTM)
  
#growth in ADL overall
Canada_daily_all %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>% 
  group_by(Date, year) %>% 
  summarise(n = n()) %>% 
  group_by(year) %>% 
  summarise(mean = mean(n)) %>% 
  spread(key = year, value = mean) %>% 
  mutate(growth = (LTM-MTM)/MTM)

## Revenue (Table 1) -----------------------------------------------------------------------------------------------------

#overall revenue 2018
Candaa_daily_red4 %>% 
  summarise(price = sum(Price[Status == "R"])) %>% 
  mutate(CAD = price*1.2957)

#revenue by CMATYPE 2018
Candaa_daily_red4 %>% 
  group_by(CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  mutate(CAD = x*1.2957) %>% 
  mutate(percent = CAD/1806222568)

#revenue in M, T, V 2018
Candaa_daily_red4 %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  summarise(x = sum(Price[Status == "R"])) %>%
  mutate(CAD = x*1.2957) %>% 
  mutate(percent = CAD/1806222568)

#revenue overall 2017
Candaa_daily_red3 %>% 
  summarise(price = sum(Price[Status == "R"])) %>% 
  mutate(CAD = price*1.2986)

#revenue by CMATYPE 2017
Candaa_daily_red3 %>% 
  group_by(CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  mutate(CAD = x*1.2986)

#revenue in M,T,V 2017
Candaa_daily_red3 %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  summarise(x = sum(Price[Status == "R"])) %>%
  mutate(CAD = x*1.2986)


#host rev paragraph------------------------------------------------------------------------------------
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(),rev = sum(Price[Status == "R"])) %>% 
  filter(rev > 0)

Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  summarise(med = median(rev)) %>% 
  mutate(CAD = med*1.2957)

Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  summarise(men = mean(rev))%>% 
  mutate(CAD = men*1.2957)

#calculated host percentiles using viz3 (fig5)

## multilistings------------------------------------------------------------------------------
#revenue derived from ML by CMATYPE 2018
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))

#revenue derived from ML by CMATYPE 2017
Canada_daily_all %>% 
  ungroup() %>% 
  filter(Date <="2017-12-31", Date >= "2017-01-01") %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))

#percent of ML on last day of  2018 by CMATYPE
Candaa_daily_red4 %>% 
  ungroup() %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(ML == TRUE)/n())

#percent of ML on last day of 2017 by CMATYPE
Canada_daily_all %>% 
  ungroup() %>% 
  filter(Date == "2017-12-31") %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(ML == TRUE)/n())

#percent of revenue derived from ML by CMANAME 2018
ML_CMA <- Candaa_daily_red4 %>% 
  ungroup() %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(CMANAME) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))

#growth in percent of revenue derived from ML by CMANAME 2017 --> 2018
growth <- Canada_daily_all %>% 
  ungroup() %>% 
  filter(CMATYPE == "CMA") %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>%  
  group_by(year, CMANAME) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"])) %>% 
  spread(key = year, value = percent) %>% 
  mutate(growth = (LTM-MTM))

#hosts greater than $1,000,000 revenue in 2018
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Host_ID, Property_ID) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(), rev = sum(rev)) %>% 
  mutate(revCAD = rev*1.2957) %>% 
  filter(revCAD>1000000)

#number of properties per host?
host <- Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(Host_ID, Property_ID) %>% 
  summarise(n = n()) %>% 
  group_by(Host_ID) %>% 
  summarise(n = n())

#percent of listings that are entire homes 2018
Candaa_daily_red4 %>% 
  group_by(Property_ID, CMATYPE, Listing_Type) %>% 
  summarise(n = n(), x = sum(Price[Status == "R"]), 
            y = sum(Price[Status == "R" & Listing_Type == "Entire home/apt"])) %>% 
  group_by(CMATYPE) %>% 
  summarise(percEH = sum(Listing_Type == "Entire home/apt")/n(), 
            rev = sum(y)/sum(x))

#growth in percent of EH listings
Canada_daily_all %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>%  
  group_by(Property_ID, CMATYPE, Listing_Type, year) %>% 
  summarise(n = n(), x = sum(Price[Status == "R"]), y = sum(Price[Status == "R" & Listing_Type == "Entire home/apt"])) %>% 
  group_by(CMATYPE, year) %>% 
  summarise(percEH = sum(Listing_Type == "Entire home/apt")/n(), 
            rev = sum(y)/sum(x)) %>% 
  mutate(growth = (LTM-MTM))

##FREH
#FREH by CMATYPE 2018
Canada_FREH %>% 
  ungroup() %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(FREH))

#FREH in MTV 2018
Canada_FREH %>% 
  ungroup() %>% 
  filter(Date == "2018-12-31", CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  summarise(sum(FREH))

#growth in FREH by CMATYPE
Canada_FREH %>% 
  ungroup() %>% 
  filter(Date == "2018-12-31" | Date == "2017-12-31") %>% 
  group_by(CMATYPE, Date) %>% 
  summarise(sum = sum(FREH)) %>% 
  spread(key = Date, value = sum) %>% 
  mutate(growth = (`2018-12-31` - `2017-12-31`)/`2017-12-31`)

#growth in FREH overall 
Canada_FREH %>% 
  ungroup() %>% 
  filter(Date == "2018-12-31" | Date == "2017-12-31") %>% 
  group_by(Date) %>% 
  summarise(sum = sum(FREH)) %>% 
  spread(key = Date, value = sum) %>% 
  mutate(growth = (`2018-12-31` - `2017-12-31`)/`2017-12-31`)

#growth in FREH in MTV
Canada_FREH %>% 
  ungroup() %>% 
  filter((Date == "2018-12-31" | Date == "2017-12-31") & CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(Date) %>% 
  summarise(sum = sum(FREH)) %>% 
  spread(key = Date, value = sum) %>% 
  mutate(growth = (`2018-12-31` - `2017-12-31`)/`2017-12-31`)

#list of all FREH in 2018 
prop2018FREH <- Canada_FREH %>% 
  filter(FREH == 1, Date == "2018-12-31")

#revenue earned by FREH by CMATYPE 2018
Candaa_daily_red4 %>% 
  filter(Property_ID %in% prop2018FREH$Property_ID) %>% 
  group_by(CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  mutate(CAD = x*1.2957)

#revenue earned by FREH MTV 2018
Candaa_daily_red4 %>% 
  filter(Property_ID %in% prop2018FREH$Property_ID, CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"]))%>% 
  mutate(CAD = x*1.2957)

#list of all FREH in 2017
prop2017FREH <- Canada_FREH %>% 
  filter(FREH == 1, Date == "2017-12-31")

#revenue earned by FREH by CMATYPE 2017
Canada_daily_all %>% 
  filter(Property_ID %in% prop2017FREH$Property_ID, Date <= "2017-12-31", Date>= "2017-01-01") %>% 
  group_by(CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"]))%>% 
  mutate(CAD = x*1.2986)

#revenue earned by FREH MTV 2017
Canada_daily_all %>% 
  filter(Property_ID %in% prop2017FREH$Property_ID, Date <= "2017-12-31", Date>= "2017-01-01", CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"]))%>% 
  mutate(CAD = x*1.2986)

#revenue earned overall 2017
Canada_daily_all %>% 
  filter(Property_ID %in% prop2017FREH$Property_ID, Date <= "2017-12-31", Date>= "2017-01-01") %>% 
  summarise(x = sum(Price[Status == "R"]))%>% 
  mutate(CAD = x*1.2986)

#proportion FREH
#percent of FREH by CMATYPE in 2018
Candaa_daily_red4 %>% 
  ungroup() %>% 
  group_by(CMATYPE, Property_ID, Listing_Type) %>% 
  summarise(n = n()) %>% 
  left_join(prop2018FREH, by = "Property_ID") %>% 
  group_by(CMATYPE.x) %>% 
  summarise(sum(FREH == 1, na.rm = TRUE)/n())

#percent of rev from FREH by CMATYPE in 2018
 Candaa_daily_red4 %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(Price[Status == "R"&Property_ID %in% prop2018FREH$Property_ID])/
              sum(Price[Status == "R"]))

 #percent of EH rev that is from FREH by CMATYPE in 2018
Candaa_daily_red4 %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(Price[Status == "R"&Property_ID %in% prop2018FREH$Property_ID])/
              sum(Price[Status == "R"& Listing_Type == "Entire home/apt"]))

#percent of listings that are FREH 2018 by CMATYPE
Canada_FREH %>% 
  ungroup() %>% 
  filter(Listing_Type == "Entire home/apt") %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(CMATYPE) %>% 
  summarise(mean(FREH))

cor(ly_daily2$`Entire homes`, ly_daily2$`Frequently-rented entire homes`, method = c("pearson"))

## percent of all housing units
library(cancensus)
Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C=c("01")), level = "CT")

#percent of dwellings that are FREH in 2018 by CMA
x <- Canada_FREH %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(CTUID, CMANAME) %>% 
  summarise(FREH = sum(FREH)) %>% 
  left_join(cd[,c(1,6)], by = c("CTUID" = "GeoUID")) %>% 
  group_by(CMANAME) %>% 
  summarise(FREH = sum(FREH), units = sum(Dwellings)) %>% 
  mutate(percEH = FREH/units)

#percent of EH units in CMAs and CAs
Candaa_daily_red4 %>% 
  ungroup() %>% 
  filter(CMATYPE == "CMA" | CMATYPE == "CA") %>% 
  summarise(sum(Listing_Type == "Entire home/apt")/n())

#tofino percent FREH units
cd2 <- get_census("CA16", regions = list(C=c("01")), level = "DA")
Tofino_FREH <- subset(Canada_FREH, Canada_property$winner %in% c(59230087:59230090))
Tofino_FREH <- Canada_FREH %>% 
  filter(winner %in% c(59230087:59230090))
class(Tofino_FREH$winner)
cd2$GeoUID <- as.numeric(cd2$GeoUID)
cd2 <- cd2 %>% 
  filter(GeoUID%in% c(59230087:59230090))
Tofino_FREH %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(winner) %>% 
  summarise(FREH = sum(FREH)) %>% 
  full_join(cd2[,c(1,6)], by= c("winner" = "GeoUID")) %>% 
  summarise(FREH = sum(FREH,na.rm = TRUE), Dwellings = sum(Dwellings, na.rm = TRUE)) %>% 
  mutate(FREH/Dwellings)

#Lethbridge percent FREH units
cd3 <- get_census("CA16", regions = list(C=c("01")), level = "CT")
cd3$GeoUID <- as.numeric(cd3$GeoUID)
cd3$GeoUID <- as.character(cd3$GeoUID)
cd3 <- cd3 %>% 
  filter(`Region Name` == "Lethbridge")

Canada_FREH %>% 
  filter(CMANAME %in% c("Lethbridge"), Date == "2018-12-31") %>% 
  group_by(CTUID) %>% 
  summarise(FREH = sum(FREH)) %>% 
  full_join(cd3[,c(1,6)], by= c("CTUID" = "GeoUID")) %>% 
  summarise(FREH = sum(FREH,na.rm = TRUE), Dwellings = sum(Dwellings, na.rm = TRUE)) %>% 
  mutate(FREH/Dwellings)