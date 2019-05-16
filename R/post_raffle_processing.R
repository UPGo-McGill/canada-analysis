## Post Raffle Processing
library(tidyverse)
library(sf)
library(foreign)
library(cancensus)

property <- read_csv("data/Canada_property.csv")
Canada_daily <- read_csv("data2/Canada_daily.csv")

Canada_daily <- Canada_daily[, c(1:14)]
Canada_property <- Canada_property[, c(1:21)]

# get/clean additional geography attributes
DA <- read.dbf("data/DA.dbf", as.is = TRUE)
DA$CMANAME <- parse_character(DA$CMANAME, locale = locale(encoding = "Latin1"))
DA$CSDNAME <- parse_character(DA$CSDNAME, locale = locale(encoding = "Latin1"))
DA$DAUID <- as.numeric(DA$DAUID)
DA <- DA %>% mutate(CMATYPE = coalesce(CMATYPE, "Rural"))
DA <- DA %>% mutate(CMATYPE = str_replace(CMATYPE, "B", "CMA"))
DA <- DA %>% mutate(CMATYPE = str_replace(CMATYPE, "D", "CA"))
DA <- DA %>% mutate(CMATYPE = str_replace(CMATYPE, "K", "CA"))
DA[DA$CMANAME%in%"Lloydminster (Saskatchewan part / partie de la Saskatchewan)",]$CMANAME <- c("Lloydminster")
DA[DA$CMANAME%in%"Lloydminster (Alberta part / partie de l'Alberta)",]$CMANAME <- c("Lloydminster")
DA[DA$CMANAME%in%"Greater Sudbury / Grand Sudbury",]$CMANAME <- c("Greater Sudbury")
DA[DA$CMANAME%in%"Hawkesbury (Ontario part / partie de l'Ontario)",]$CMANAME <- c("Hawkesbury")
DA[DA$CMANAME%in%"Hawkesbury (partie du Québec / Quebec part)" ,]$CMANAME <- c("Hawkesbury")
DA[DA$CMANAME%in%"Ottawa - Gatineau (Ontario part / partie de l'Ontario)",]$CMANAME <- c("Ottawa - Gatineau")
DA[DA$CMANAME%in%"Ottawa - Gatineau (partie du Québec / Quebec part)",]$CMANAME <- c("Ottawa - Gatineau")
DA[DA$CMANAME%in%"Campbellton (New Brunswick part / partie du Nouveau-Brunswick)",]$CMANAME <- c("Campbellton")
DA[DA$CMANAME%in%"Campbellton (partie du Québec / Quebec part)",]$CMANAME <- c("Campbellton")

#get CT population
Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C=c("01")), level = "CT")
cd <- cd %>% 
  select(GeoUID, Population) %>% 
  rename(CTUID = GeoUID, CT_pop = Population)

#get CMA population
popCMA <- read_csv("data/CMA.csv")
colnames(popCMA) <- c("CMANAME", "CMA_pop")

#join population to geog attributes
DA <- DA %>% 
  left_join(cd, by = "CTUID") %>% 
  left_join(popCMA, by = "CMANAME")
rm(cd)
rm(popCMA)


# join to prop file
Canada_property <- Canada_property %>% 
  left_join(DA[,c(1,3,10,17:20,22:24)], by = c("winner" = "DAUID"))
rm(DA)

#join to daily file
Canada_daily <- daily %>%  #merge daily file with raffled geog attributes
  left_join(Canada_property[,c(1,20,22, 25:27,29:30)], by = "Property_ID")





## to get CMA pop from cancensus
cd <- cd %>% 
  mutate(prac2 = `Region Name`) %>% 
  mutate(prac2 = str_replace(prac2, "[ ][(].[)]$", "")) %>% 
  mutate(prac2 = str_replace(prac2, "[ ][(]..[)]$", "")) %>% 
  mutate(prac2 = str_replace(prac2, "[ ][(]...[)]$", "")) %>% 
  select(prac2, Population) %>% 
  rename(CMANAME = prac2) %>% 
  arrange(CMANAME) %>% 
  slice(c(-70,-71,-214,-473,-767, -1462,-2098, -2586, -2650, -3010,-3841,-4065,-4066, -4191, -4349:-4351, -4531:-4532, -4563:-4565))
cd[cd$CMANAME=="Lloydminster (Part)",]$CMANAME <- c("Lloydminster")
cd[cd$CMANAME=="Norfolk County",]$CMANAME <- c("Norfolk")
cd[cd$CMANAME=="Campbellton",]$CMANAME <- c("Campbellton") #other cleaning of cancensus file
cd[cd$CMANAME=="Lloydminster",]$Population <- c(34583)
cd[cd$CMANAME=="Nelson",]$Population <- c(18307)
raffled <- raffled %>% 
  left_join(cd, by = "CMANAME")


Canada_daily <- Canada_daily %>% mutate(CMATYPE = coalesce(CMATYPE, "Rural"))
Canada_daily <- Canada_daily %>% mutate(CMATYPE = str_replace(CMATYPE, "B", "CMA"))
Canada_daily <- Canada_daily %>% mutate(CMATYPE = str_replace(CMATYPE, "D", "CA"))
Canada_daily <- Canada_daily %>% mutate(CMATYPE = str_replace(CMATYPE, "K", "CA"))
#old way of changing cmaname
DA[DA$CMANAME%in%"Lloydminster (Saskatchewan part / partie de la Saskatchewan)",]$CMANAME <- c("Lloydminster")
DA[DA$CMANAME%in%"Lloydminster (Alberta part / partie de l'Alberta)",]$CMANAME <- c("Lloydminster")
DA[DA$CMANAME%in%"Greater Sudbury / Grand Sudbury",]$CMANAME <- c("Greater Sudbury")
DA[DA$CMANAME%in%"Hawkesbury (Ontario part / partie de l'Ontario)",]$CMANAME <- c("Hawkesbury")
DA[DA$CMANAME%in%"Hawkesbury (partie du Québec / Quebec part)" ,]$CMANAME <- c("Hawkesbury")
DA[DA$CMANAME%in%"Ottawa - Gatineau (Ontario part / partie de l'Ontario)",]$CMANAME <- c("Ottawa - Gatineau")
DA[DA$CMANAME%in%"Ottawa - Gatineau (partie du Québec / Quebec part)",]$CMANAME <- c("Ottawa - Gatineau")
DA[DA$CMANAME%in%"Campbellton (New Brunswick part / partie du Nouveau-Brunswick)",]$CMANAME <- c("Campbellton")
DA[DA$CMANAME%in%"Campbellton (partie du Québec / Quebec part)",]$CMANAME <- c("Campbellton")
