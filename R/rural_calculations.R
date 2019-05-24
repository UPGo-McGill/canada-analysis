





#Stats---------------------------------------------------------------------------------------------------------------------


csd_units <- get_census("CA16", regions = list(C = c("01")), level = 'CSD')
csd_units <- csd_units %>% 
  select(GeoUID, Dwellings, Population) %>% 
  mutate(CSD = ifelse(GeoUID == 5931020, "Whistler",
                      ifelse(GeoUID == 3513020, "Prince Edward County",
                             ifelse(GeoUID == 2478102, "Mont-Tremblant",
                                    ifelse(GeoUID == 3542045, "The Blue Mountains", "Other"))))) %>% 
  group_by(CSD) %>% 
  summarise(Dwellings = sum(Dwellings), 
            Population = sum(Population))

rural <- Canada_daily %>% 
  filter(CMATYPE == "Rural",
         Date >= "2017-01-01",
         Date <= "2018-12-31",
         Housing == TRUE) %>% 
  left_join(Canada_property[,c(1,20,21,22,30)], by = "Property_ID") %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA)),
         CSD = ifelse(CSDUID == 5931020, "Whistler",
                      ifelse(CSDUID == 3513020, "Prince Edward County",
                             ifelse(CSDUID == 2478102, "Mont-Tremblant",
                                    ifelse(CSDUID == 3542045, "The Blue Mountains", "Other"))))) %>% 
  group_by(Date, year, CSD) %>% 
  summarize(n = n(), rev = sum(Price[Status == "R"])) %>% 
  group_by(CSD, year) %>% 
  summarise(ADL = mean(n), rev = sum(rev))

rural2 <- dcast(setDT(rural), CSD ~ year, value.var = c("ADL", "rev"), fun = sum) 

rural2 <- rural2 %>%
  left_join(csd_units, by = "CSD") %>% 
  mutate(`listings/dwelling` = ADL_LTM/Dwellings,
         `rev/dwelling` = rev_LTM/Dwellings,
         growth_rev = (rev_LTM - rev_MTM)/rev_MTM,
         growth_listings = (ADL_LTM - ADL_MTM)/ADL_MTM,
         percent_listings = ADL_LTM/sum(ADL_LTM),
         percent_rev = rev_LTM/sum(rev_LTM))

write_csv(rural2, "rural_stats.csv")

rural2 <- rural2 %>% 
  mutate(revlisting = rev_LTM/ADL_LTM)

mean(graph2$percentFREH, na.rm = TRUE)

#Old stuff------------------------------------------------------------------------------------------------------------------
rural <- rural %>% 
  left_join(Canada_property[,c(1,20,21,22,30)], by = "Property_ID")

rural_csd <- rural %>% 
  group_by(CSDUID, CSDNAME, PRNAME) %>% 
  summarize(listings = n(), 
            rev = sum(rev), 
            listings2017 = sum(`2017`==1), 
            rev2017 = sum(rev2017),
            listings2018 = sum(`2018`==1),
            rev2018 = sum(rev2018))



rural_csd <- rural_csd %>% 
  left_join(csd_units, by = c("CSDUID" = "GeoUID"))

rural_csd <- rural_csd %>% 
  mutate(listings_per_unit = listings/Dwellings) %>% 
  mutate(rev_per_unit = rev/Dwellings)

ggplot(data = rural_csd, aes(x = Population, y = listings))+
  geom_point()+
  geom_smooth()


rural_csd <- rural_csd %>% 
  mutate(CSD = ifelse(CSDUID == 5931020, "Whistler",
                      ifelse(CSDUID == 3513020, "Prince Edward County",
                             ifelse(CSDUID == 2478102, "Mont-Tremblant",
                                    ifelse(CSDUID == 3542045, "The Blue Mountains", "Other")))))


stats <- rural_csd %>% 
  group_by(CSD) %>%
  summarise(`# of listings` = sum(listings),
            `Revenue` = sum(rev),
            `Listings/dwellings` = sum(listings)/sum(Dwellings), 
            `Listings/population` = sum(listings)/sum(Population), 
            `Revenue/listings` = sum(rev)/sum(listings), 
            `Revenue/Dwellings` = sum(rev)/sum(Dwellings),
            listings2017 = sum(listings2017),
            listings2018 = sum(listings2018),
            rev2017 = sum(rev2017),
            rev2018 = sum(rev2018)) %>% 
  mutate(`% of listings` = `# of listings`/sum(`# of listings`),
         `% of revenue` = `Revenue`/sum(`Revenue`),
         growthlistings = (listings2018-listings2017)/listings2017,
         growthrev = (rev2018-rev2017)/rev2017)

(sum(stats$listings2018) - sum(stats$listings2017))/sum(stats$listings2017)


#________________________________________
cd <- get_census("CA16", regions = list(C = c("01")), level = 'CSD') #get CTs with population
cd <- cd %>% 
  select(GeoUID, Dwellings, `Region Name`, PR_UID) %>% 
  rename(units = Dwellings) %>% 
  mutate(CSDNAME = `Region Name`) %>% 
  mutate(CSDNAME = str_replace(CSDNAME, "[ ][(].[)]$", "")) %>% 
  mutate(CSDNAME = str_replace(CSDNAME, "[ ][(]..[)]$", "")) %>% 
  mutate(CSDNAME = str_replace(CSDNAME, "[ ][(]...[)]$", "")) %>% 
  mutate(CSDNAME = str_replace(CSDNAME, "[ ][(]....[)]$", "")) %>% 
  mutate(CSDNAME = str_replace(CSDNAME, "[ ][(].....[)]$", "")) %>% 
  mutate(CSDNAME = str_replace(CSDNAME, "[ ][(]......[)]$", ""))

rural <- rural %>% 
  mutate(PR_UID = ifelse(PRNAME == "British Columbia / Colombie-Britannique", 59,
                         ifelse(PRNAME == "Ontario", 35,
                                ifelse(PRNAME == "Quebec / Qu\xe9bec", 24,
                                       ifelse(PRNAME == "Nova Scotia / Nouvelle-\xc9cosse", 12,
                                              ifelse(PRNAME == "Alberta", 48,
                                                     ifelse(PRNAME == "Newfoundland and Labrador / Terre-Neuve-et-Labrador", 10,
                                                            ifelse(PRNAME == "New Brunswick / Nouveau-Brunswick", 13,
                                                                   ifelse(PRNAME == "Prince Edward Island / \xcele-du-Prince-\xc9douard", 11,
                                                                          ifelse(PRNAME == "Manitoba",46, 
                                                                                 ifelse(PRNAME == "Saskatchewan", 47,
                                                                                        ifelse(PRNAME == "Nunavut", 62,
                                                                                               ifelse(PRNAME == "Northwest Territories / Territoires du Nord-Ouest", 61,
                                                                                                      ifelse(PRNAME == "Yukon", 60, NA))))))))))))))


cd$PR_UID <- as.numeric(cd$PR_UID)
rural <- rural %>% 
  left_join(cd, by = c("CSDNAME","PR_UID"))
rural <- rural %>% 
  select(-`Region Name`, -geometry)








