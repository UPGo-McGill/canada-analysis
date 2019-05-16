


x <- FREH_CSD %>% 
  filter(Date == "2019-01-28" | Date == "2018-01-28") %>% 
  group_by(CMATYPE, Date) %>% 
  summarise(n = sum(n)) %>% 
  spread(key = Date, value = n) %>% 
  mutate(growth = (`2019-01-28`-`2018-01-28`)/`2018-01-28`)

(sum(x$`2019-01-28`) - sum(x$`2018-01-28`))/sum(x$`2018-01-28`)

FREH_CSD %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver") &
         (Date == "2019-01-28" | Date == "2018-01-28")) %>% 
  group_by(CMATYPE, Date) %>% 
  summarise(n = sum(n)) %>% 
  spread(key = Date, value = n) %>% 
  mutate(growth = (`2019-01-28`-`2018-01-28`)/`2018-01-28`)


Candaa_daily_red2 %>% 
  ungroup() %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(n = n()) %>% 
  filter(n>=365) %>% 
  group_by(CMATYPE) %>% 
  summarise(n = n())

colnames(Canada_daily)


sum(Canada_daily$MLnew == TRUE)
sum(Canada_daily$MLnew == FALSE)

test <- Canada_daily %>% 
  ungroup() %>% 
  filter(ML == TRUE) %>% 
  group_by(CMATYPE) %>% 
  summarise(n = n())

test <- Canada_daily %>% 
  filter(Date <="2019-01-28", Date >= "2018-01-29", Status == "R")

Candaa_daily_red3 <- Canada_daily %>% 
  ungroup() %>% 
  filter(Date<="2019-01-28", Date >= "2018-01-29", Housing == TRUE)

## Multilistings -----------------------------------------------------------------------------

Candaa_daily_red3 %>% 
  ungroup() %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))

Canada_daily %>% 
  ungroup() %>% 
  filter(Date <="2018-01-28", Date >= "2017-01-29") %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))

Candaa_daily_red3 %>% 
  ungroup() %>% 
  filter(Date == "2019-01-28") %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(ML == TRUE)/n())

Canada_daily %>% 
  ungroup() %>% 
  filter(Date == "2018-01-28") %>% 
  group_by(CMATYPE) %>% 
  summarise(percent = sum(ML == TRUE)/n())



ML_CMA <- Candaa_daily_red3 %>% 
  ungroup() %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(CMANAME) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))


growth <- Canada_daily %>% 
  ungroup() %>% 
  filter(CMATYPE == "CMA") %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(year, CMANAME) %>% 
  summarise(percent = sum(Price[Status == "R" & MLnew == TRUE])/sum(Price[Status == "R"]))
growth_spread <- spread(growth, key = year, value = percent)
growth_spread <- growth_spread %>% 
  mutate(growth = (LTM-MTM)/MTM)


host <- Candaa_daily_red3 %>% 
  ungroup() %>% 
  group_by(Host_ID, Property_ID) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(), rev = sum(rev)) %>% 
  mutate(revCAD = rev*1.2957)

host %>% 
  filter(revCAD>1000000)

# host revenue paragraph
Candaa_daily_red3 %>% 
  summarise(sum(Price[Status == "R"]))

Canada_daily %>% 
  filter(Date >= "2017-01-29" & Date <= "2018-01-28") %>% 
  summarise(sum(Price[Status == "R"]))

Candaa_daily_red3 %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  summarise(median(rev))

#cmhc stuff
Candaa_daily_red3 %>% 
  

