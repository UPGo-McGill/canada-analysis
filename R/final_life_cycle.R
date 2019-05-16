## Activity Period

#PPN-----------------------------------------------------------------------------------

#subset
num_month <- Canada_property %>% 
  filter(Created>="2016-09-01") %>% 
  mutate(month = format(as.Date(Created), "%m")) %>%
  group_by(month) %>% 
  summarise(n = n())
feb <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "02")
mar <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "03")
apr <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "04")
may <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "05")
jun <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "06")
jul <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "07")
aug <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "08")
sep <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "09")
oct <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "10")
nov <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "11")
dec <- Canada_property %>% 
  mutate(month = format(as.Date(Created), "%m")) %>% 
  filter(Created>="2016-09-01" & month == "12")

jansubset <- sample(jan$Property_ID, size = 11877, replace = FALSE)
febsubset <- sample(feb$Property_ID, size = 11877, replace = FALSE)
marsubset <- sample(mar$Property_ID, size = 11877, replace = FALSE)
aprsubset <- sample(apr$Property_ID, size = 11877, replace = FALSE)
maysubset <- sample(may$Property_ID, size = 11877, replace = FALSE)
junsubset <- sample(jun$Property_ID, size = 11877, replace = FALSE)
julsubset <- sample(jul$Property_ID, size = 11877, replace = FALSE)
augsubset <- sample(aug$Property_ID, size = 11877, replace = FALSE)
sepsubset <- sample(sep$Property_ID, size = 11877, replace = FALSE)
octsubset <- sample(oct$Property_ID, size = 11877, replace = FALSE)
novsubset <- sample(nov$Property_ID, size = 11877, replace = FALSE)
decsubset <- sample(dec$Property_ID, size = 11877, replace = FALSE)

activity_subset <-c(jansubset, febsubset, marsubset, aprsubset, maysubset, junsubset,
                    julsubset, augsubset, sepsubset, octsubset, novsubset, decsubset)

as <- Canada_daily %>% 
  filter(Property_ID %in% activity_subset)


test <- sample(Canada_property$Property_ID, size = 11877, replace = FALSE)
test2<- Canada_daily %>% 
  filter(Property_ID %in% test)


### limit based on month of first reservation ---------------------------------------

Canada_daily <- Canada_daily %>% 
  mutate(month.res = ifelse (day_num_r == 1, format(as.Date(Date), "%m"), NA))

month.res <- Canada_daily %>% 
  filter(Created >= "2016-09-01") %>% 
  group_by(month.res) %>% 
  summarise(n = n())

jan <- Canada_daily %>%
  filter(Created>="2016-09-01" & month.res == "01")
feb <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "02")
mar <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "03")
apr <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "04")
may <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "05")
jun <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "06")
jul <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "07")
aug <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "08")
sep <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "09")
oct <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "10")
nov <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "11")
dec <- Canada_daily %>% 
  filter(Created>="2016-09-01" & month.res == "12")

jansubset <- sample(jan$Property_ID, size = 6469, replace = FALSE)
febsubset <- sample(feb$Property_ID, size = 6469, replace = FALSE)
marsubset <- sample(mar$Property_ID, size = 6469, replace = FALSE)
aprsubset <- sample(apr$Property_ID, size = 6469, replace = FALSE)
maysubset <- sample(may$Property_ID, size = 6469, replace = FALSE)
junsubset <- sample(jun$Property_ID, size = 6469, replace = FALSE)
julsubset <- sample(jul$Property_ID, size = 6469, replace = FALSE)
augsubset <- sample(aug$Property_ID, size = 6469, replace = FALSE)
sepsubset <- sample(sep$Property_ID, size = 6469, replace = FALSE)
octsubset <- sample(oct$Property_ID, size = 6469, replace = FALSE)
novsubset <- sample(nov$Property_ID, size = 6469, replace = FALSE)
decsubset <- sample(dec$Property_ID, size = 6469, replace = FALSE)


month.res.subset <-c(jansubset, febsubset, marsubset, aprsubset, maysubset, junsubset,
                    julsubset, augsubset, sepsubset, octsubset, novsubset, decsubset)

month.res.sub <- Canada_daily %>% 
  filter(Property_ID %in% month.res.subset)


#Overall

ppnmed_overall <-Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r) %>% 
  summarise(ppn = median(Price), n = n())

#CMATYPE
ppnmed_cmatype <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-10-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, CMATYPE) %>% 
  summarise(ppn = median(Price), n = n())

#FREH
ppnmed_freh <- Canada_daily %>% 
  filter(Status == "R", Created >= "2016-10-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, FREH2) %>% 
  summarise(ppn = median(Price), n = n())

#CMANAME
ppnmed_cmaname <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-10-01", Housing == TRUE, CMATYPE == "CMA") %>% 
  group_by(day_num_r, CMANAME) %>% 
  summarise(ppn = median(Price), n = n())

#LT
ppnmed_lt <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-10-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, Listing_Type) %>% 
  summarise(ppn = median(Price), n = n())


