library(tidyverse)

Canada_daily_red <- Canada_daily %>% 
  filter(Date<="2019-01-28", Date >= "2018-01-29", Housing == TRUE)

## number of listings removed from analysis cause housing
sum(Canada_property$Housing == FALSE) / nrow(Canada_property)

test <- Canada_daily %>% 
  filter(Date <="2019-01-28", Date >= "2018-01-29", Status == "R")
 
sum(test$Price[test$Housing == FALSE])/sum(test$Price) 


##unique listings with at least one booking 
uniquebooking <- Canada_daily %>% 
  filter(Housing == TRUE) %>% 
  group_by(Property_ID) %>% 
  summarise(status = sum(Status == "R")) %>% 
  filter(status>0)

##unique listings with at least one booking LTM
uniquebookingLTM <- Canada_daily_red %>% 
  group_by(Property_ID) %>% 
  summarise(status = sum(Status == "R")) %>% 
  filter(status>0)

hostperc <- Candaa_daily_red2 %>% #determining least/most concentrated cmas for the lines
  filter(Status == "R") %>% 
  group_by(Host_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  summarise(one = quantile(rev, c(0.99)), oneperc = sum(rev[rev>one]), `1%` = oneperc/sum(rev),
            five = quantile(rev, c(0.95)), fiveperc = sum(rev[rev>five]), `5%` = fiveperc/sum(rev), 
            ten = quantile(rev, c(0.90)), tenperc = sum(rev[rev>ten]), `10%` = tenperc/sum(rev)) %>% 
  select(`1%`, `5%`, `10%`) %>% 
  gather(`1%`, `5%`, `10%`, key = "percentile", value = "value")

##------------------------------------Table 1--------------------------------------------------------
#active listings overall
active <- Candaa_daily_red2 %>% 
  group_by(Property_ID, Date) %>% 
  summarise(n = n()) %>% 
  group_by(Date) %>% 
  summarise(n = n()) %>% 
  summarise(mean(n))


#active listings CMATYPE
activeLTM <- Canada_daily %>% 
  filter(Date<="2019-01-28", Date >= "2018-01-29", Housing == TRUE) %>% 
  group_by(Property_ID, CMATYPE, Date) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE, Date) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>%
  summarise(mean(n))

#active listings MTV
activeLTM_MTV <- Candaa_daily_red2 %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(Property_ID, Date) %>% 
  summarise(n = n()) %>% 
  group_by(Date) %>% 
  summarise(n = n()) %>% 
  summarise(mean(n))

#active listings growth CMATYPE
growth_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year, CMATYPE) %>% 
  summarise(n = n()) %>% 
  group_by(year, CMATYPE) %>% 
  summarise(mean = mean(n))
growth_type_spread <- spread(growth_cmatype, key = year, value = mean)
growth_type_spread <- growth_type_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

#active listings growth MTV
growth_mtv <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year, CMANAME) %>% 
  summarise(n = n()) %>% 
  group_by(year, CMANAME) %>% 
  summarise(mean = mean(n))
growth_type_spread_mtv <- spread(growth_mtv, key = year, value = mean)
growth_type_spread_mtv <- growth_type_spread_mtv %>% 
  mutate(growth = (LTM-MTM)/MTM)
growth_mtv2 <- growth_type_spread_mtv %>% 
  summarise(LTM = sum(LTM), MTM = sum(MTM))

#active listings growth overall
growth_overall <- Canada_daily %>% 
  filter(Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year) %>% 
  summarise(n = n()) %>% 
  group_by(year) %>% 
  summarise(mean = mean(n))
growth_overall_spread <- spread(growth_overall, key = year, value = mean)
growth_overall_spread <- growth_overall_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

#Rev CMATYPE
rev_cmatype <- Candaa_daily_red2 %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(Price[Status == "R"]))

#Rev MTV
rev_mtv <- Canada_daily_red %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(CMANAME) %>% 
  summarise(sum(Price[Status == "R"]))

#MTV rev growth
rev_growth_mtv <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(year, CMANAME) %>% 
  summarise(sum(Price[Status == "R"]))
rev_growth_type_spread_mtv <- spread(rev_growth_mtv, key = year, value = `sum(Price[Status == "R"])`)
rev_growth_type_spread_mtv <- rev_growth_type_spread_mtv %>% 
  mutate(growth = (LTM-MTM)/MTM)
growth_mtv2 <- rev_growth_type_spread_mtv %>% 
  summarise(LTM = sum(LTM), MTM = sum(MTM))

#CMATYPE rev growth
rev_growth <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE)) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(year, CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"]))
rev_growth <- spread(rev_growth, key = year, value = x)
rev_growth <- rev_growth %>% 
  mutate(growth = (LTM-MTM)/MTM)

#Overall rev growth
rev_growth_overall <- Canada_daily %>% 
  filter(Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(year) %>% 
  summarise(sum(Price[Status == "R"]))
rev_growth_overall_spread <- spread(rev_growth_overall, key = year, value = mean)
rev_growth_overall_spread <- rev_growth_overall_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

##Page 11--------------------------------------------------------
#PPN

ppnmed <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>%
  group_by(CMATYPE) %>% 
  summarise(ppnmed = median(Price), ppnavg = mean(Price), n = n())


#occupancy rate
occ_rate <- Canada_daily %>% 
  filter(Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>%
  group_by(CMATYPE) %>% 
  summarise(sum(Status == "R")/n())

##Hosts with more than 100 listings---------------------------------------

hosts <- Canada_daily_red %>% 
  group_by(Host_ID, Property_ID) %>% 
  summarise(n = n(), x = sum(Price[Status == "R"])) %>% 
  group_by(Host_ID) %>% 
  summarise(n(), sum(x))

## Entire homes ------------------------------------------------------------
eh_percent_LTM <- Candaa_daily_red2 %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(Property_ID, CMATYPE, Listing_Type) %>% 
  summarise(n = n(), x = sum(Price[Status == "R"]), 
            y = sum(Price[Status == "R" & Listing_Type == "Entire home/apt"])) %>% 
  group_by(CMATYPE) %>% 
  summarise(percEH = sum(Listing_Type == "Entire home/apt")/n(), 
            rev = sum(y)/sum(x))

eh_percent_LTM_growth <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Property_ID, CMATYPE, Listing_Type, year) %>% 
  summarise(n = n(), x = sum(Price[Status == "R"]), y = sum(Price[Status == "R" & Listing_Type == "Entire home/apt"])) %>% 
  group_by(CMATYPE, year) %>% 
  summarise(percEH = sum(Listing_Type == "Entire home/apt")/n(), 
            rev = sum(y)/sum(x))
rev_growth_overall_spread <- spread(rev_growth_overall, key = year, value = mean)
rev_growth_overall_spread <- rev_growth_overall_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

##FREH-----------------------------------------------------------------------
Canada_daily <-
    Canada_daily %>% 
    arrange(Property_ID, Date) %>%
    group_by(Property_ID) %>%
    mutate(
      R3652 = as.integer(cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)), # Number of R in last 365 days
      A3652 = as.integer(cumsum(Status=="A") - replace_na(lag(cumsum(Status=="A"), 365),0))) # Number of A in last 365 days
colnames(Canada_daily)

Canada_daily <- Canada_daily %>% 
  select(-R365, -A365, -Revenue365, -rev)      

Canada_daily <- Canada_daily %>% 
  mutate(FREH = ifelse(Listing_Type == "Entire home/apt" & R3652 >= 120 & R3652 + A3652 >=240 , "VFREH", 
                       ifelse(Listing_Type == "Entire home/apt" & R3652 >= 60 & R3652 + A3652 >=120, "FREH", NA)))

Candaa_daily_red2 <- Canada_daily %>% 
  filter(Date<="2019-01-28", Date >= "2018-01-29", Housing == TRUE)

colnames(Canada_daily)
colnames(Candaa_daily_red2)

  
FREH_cma2 <- Candaa_daily_red2 %>% 
  filter(!is.na(CMATYPE), Housing == TRUE) %>% 
  group_by(Date, FREH, CMATYPE) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE, FREH) %>% 
  summarise(mean(n), median(n))

FREH_cmaname <- Candaa_daily_red2 %>% 
  filter(!is.na(CMATYPE), CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(Date, FREH, CMANAME) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME, FREH) %>% 
  summarise(mean(n), median(n))

#new way 2018
FREH <- Candaa_daily_red2 %>% 
  filter(Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 120 & sum(Status == "R") + sum(Status == "A")>= 240, 1, 0)) %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(FREH))

FREH2 <- Candaa_daily_red2 %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 120 & sum(Status == "R") + sum(Status == "A")>= 240, 1, 0)) %>% 
  summarise(sum(FREH))

vFREH <- Candaa_daily_red2 %>% 
  filter(Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 60 & sum(Status == "R") + sum(Status == "A")>= 120, 1, 0)) %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(FREH))

vFREH2 <- Candaa_daily_red2 %>% 
  filter(Listing_Type == "Entire home/apt") %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 60 & sum(Status == "R") + sum(Status == "A")>= 120, 1, 0)) %>% 
  summarise(sum(FREH))

#new way 2019
FREHa <- Canada_daily %>% 
  filter(Date <= "2018-01-28", Date >= "2017-01-29", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 120 & sum(Status == "R") + sum(Status == "A")>= 240, 1, 0)) %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(FREH))

FREH2a <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"),Date <= "2018-01-28", Date >= "2017-01-29", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 120 & sum(Status == "R") + sum(Status == "A")>= 240, 1, 0)) %>% 
  summarise(sum(FREH))

vFREHa <- Canada_daily %>% 
  filter(Date <= "2018-01-28", Date >= "2017-01-29", Listing_Type == "Entire home/apt") %>%
  group_by(Property_ID, CMATYPE) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 60 & sum(Status == "R") + sum(Status == "A")>= 120, 1, 0)) %>% 
  group_by(CMATYPE) %>% 
  summarise(sum(FREH))

vFREH2a <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Date <= "2018-01-28", Date >= "2017-01-29", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 60 & sum(Status == "R") + sum(Status == "A")>= 120, 1, 0)) %>% 
  summarise(sum(FREH))

#overall 2018
vFREH2b <- Canada_daily %>% 
  filter(Date <= "2018-01-28", Date >= "2017-01-29", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 60 & sum(Status == "R") + sum(Status == "A")>= 120, 1, 0)) %>% 
  summarise(sum(FREH))

FREH2b <- Canada_daily %>% 
  filter(Date <= "2018-01-28", Date >= "2017-01-29", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  summarise(FREH = ifelse(sum(Status == "R") >= 120 & sum(Status == "R") + sum(Status == "A")>= 240, 1, 0)) %>% 
  summarise(sum(FREH))


##FREH growth
#MTV
growth_mtv_freh <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year, CMANAME, FREH) %>% 
  summarise(n = n()) %>% 
  group_by(year, CMANAME, FREH) %>% 
  summarise(mean = mean(n))
growth_mtv_freh_spread <- spread(growth_mtv_freh, key = year, value = mean)
growth_mtv_freh_spread <- growth_mtv_freh_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

#cmatype
growth_type_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year, CMATYPE, FREH) %>% 
  summarise(n = n()) %>% 
  group_by(year, CMATYPE, FREH) %>% 
  summarise(mean = mean(n))
growth_type_freh_spread <- spread(growth_type_freh, key = year, value = mean)
growth_type_freh_spread <- growth_type_freh_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

#overall
growth_freh <- Canada_daily %>% 
  filter(Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year, FREH) %>% 
  summarise(n = n()) %>% 
  group_by(year, FREH) %>% 
  summarise(mean = mean(n))
growth_freh_spread <- spread(growth_freh, key = year, value = mean)
growth_freh_spread <- growth_freh_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

##Proportion of FREH listings
prop_frehrev_type <- Candaa_daily_red2 %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(CMATYPE, Date, FREH) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  group_by(CMATYPE, FREH) %>% 
  summarise(sum(x))


growth_revfreh <- Canada_daily %>% 
  filter(Housing == TRUE) %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(Date, year, FREH, CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  group_by(year, FREH, CMATYPE) %>% 
  summarise(sum(x))
growth_freh_spread <- spread(growth_freh, key = year, value = mean)
growth_freh_spread <- growth_freh_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)

##prop FREH revenue

cmanamfreh <- Candaa_daily_red2 %>% 
  filter(CMATYPE == "CMA", Status == "R") %>% 
  group_by(CMANAME) %>% 
  summarise((sum(Price[FREH == "FREH"], na.rm = TRUE)+sum(Price[FREH == "VFREH"], na.rm = TRUE))/sum(Price, na.rm = TRUE))


## freh as percent of total housing units
library(cancensus)
Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C=c("01")), level = "CMA")

cd <- cd %>% 
  filter(Type == "CMA") %>% 
  mutate(CMA = str_replace(`Region Name`, "[ ][(].[)]$", "")) %>% 
  select(CMA, Dwellings)

FREH_cmaname2 <- Candaa_daily_red2 %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver", "Calgary", "Ottawa - Gatineau")) %>% 
  group_by(Date, FREH, CMANAME) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME, FREH) %>% 
  summarise(mean(n))

FREH_cmaname2 <- FREH_cmaname2 %>% 
  left_join(cd, by = c("CMANAME" = "CMA")) %>% 
  mutate(perc = `mean(n)`/Dwellings)

##Lethbridge
FREH_lethbridge <- Candaa_daily_red2 %>% 
  filter(CMANAME %in% c("Lethbridge")) %>% 
  group_by(Date, FREH, CMANAME) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME, FREH) %>% 
  summarise(mean(n))

FREH_lethbridge <- FREH_lethbridge %>% 
  left_join(cd, by = c("CMANAME" = "CMA")) %>% 
  mutate(perc = `mean(n)`/Dwellings)

##Tofino
Tofino_prop <- subset(Canada_property, Canada_property$winner %in% c(59230087:59230090))
Tofino_daily <- Canada_daily[Canada_daily$Property_ID %in% Tofino_prop$Property_ID,]


FREH_tofino <- Tofino_daily %>% 
  group_by(Date, FREH, CMANAME) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME, FREH) %>% 
  summarise(mean(n))

##Vacancy Rate
vacrate <- Candaa_daily_red2 %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(CMANAME, CTUID, Date, FREH) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME, CTUID, FREH) %>% 
  summarise(x = mean(n))
vacrate <- spread(vacrate, key = FREH, value = x)

Fig9 <- read_csv("data/Fig9.csv")
