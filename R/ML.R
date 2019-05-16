## Created
created_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, MLnew, month) %>% 
  summarise(n = n()) %>% 
  group_by(MLnew, month) %>% 
  summarise(n = n()) %>% 
  group_by(MLnew) %>% 
  mutate(percent = n/sum(n))

## Ramp up
r_diff2_ml <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month,MLnew) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, MLnew) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_ml_avg <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, MLnew) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(MLnew) %>% 
  summarise(mean(x, na.rm = TRUE))


## Price per night
ppnmed_ml <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r,MLnew) %>% 
  summarise(ppn = median(Price), n = n())

ppnmed_ml_avg <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(MLnew) %>% 
  summarise(ppn = median(Price), n = n())


## Occupancy rate
occ_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(MLnew)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  group_by(ML2, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_ml_avg <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(MLnew) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

## Percent with reservation
perc_res_ml <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>% 
  group_by(Property_ID, MLnew) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(MLnew) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

## Reservation length
res_length_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(MLnew, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(MLnew) %>% 
  summarise(mean(n))

## Wind-down
winddown_ml <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  group_by(Property_ID, MLnew) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(MLnew) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

## Dead
dead_ml <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(MLnew, month) %>% 
  summarise(n = n()) %>% 
  group_by(MLnew) %>% 
  mutate(percent = n/sum(n))

## Revenue
rev_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  group_by(Property_ID, MLnew) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(MLnew) %>% 
  summarise(median(rev), n = n())

## Life length
length_ml <- Canada_daily %>%
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25") %>% 
  group_by(MLnew) %>% 
  summarise(life = median(lifelength), life2 = mean(lifelength))

