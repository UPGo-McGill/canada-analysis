Canada_property$PRNAME <- Canada_property$PRNAME

Canada_daily_ATL <- 
  Canada_daily %>% 
  filter(PRNAME %in% c("Nova Scotia / Nouvelle-\xc9cosse", 
                       "Newfoundland and Labrador / Terre-Neuve-et-Labrador",
                       "Prince Edward Island / \xcele-du-Prince-\xc9douard",
                       "New Brunswick / Nouveau-Brunswick"))

Canada_daily_other <- 
  Canada_daily %>% 
  filter(!PRNAME %in% c("Nova Scotia / Nouvelle-\xc9cosse", 
                       "Newfoundland and Labrador / Terre-Neuve-et-Labrador",
                       "Prince Edward Island / \xcele-du-Prince-\xc9douard",
                       "New Brunswick / Nouveau-Brunswick"), !is.na(PRNAME))


ts_atl <- Canada_daily_ATL %>% 
    mutate(month = as.yearmon(Date)) %>% 
    group_by(month) %>% 
    summarise(rev = sum(Price)) %>% 
    filter(month<= "Dec 2018" & month>= "Jan 2017") %>% 
    ts(frequency = 12)
  ts_atl <- ts_atl[,2]
  decompose_ts_atl <- decompose(ts_atl, "multiplicative")
  
ts_other <- Canada_daily_other %>% 
    mutate(month = as.yearmon(Date)) %>% 
    group_by(month) %>% 
    summarise(rev = sum(Price)) %>% 
    filter(month<= "Dec 2018" & month>= "Jan 2017") %>% 
    ts(frequency = 12)
ts_other <- ts_other[,2]
decompose_ts_other <- decompose(ts_other, "multiplicative")

decompose_ts_atl$seasonal <- decompose_ts_atl$seasonal[1:12]  
decompose_ts_atl$seasonal <- as.numeric(decompose_ts_atl$seasonal)
decompose_ts_other$seasonal <- decompose_ts_other$seasonal[1:12]
decompose_ts_other$seasonal <- as.numeric(decompose_ts_other$seasonal)


month <- c("January", "February", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December")
month <- factor(month, levels = c("January", "February", "March", "April", "May", "June", "July",
                                  "August", "September", "October", "November", "December"))
atl <- tibble(month, decompose_ts_atl$seasonal)
other <- tibble(month, decompose_ts_other$seasonal)
atl <- atl %>% 
  rename(seasonal = `decompose_ts_atl$seasonal`)
other <- other %>% 
  rename(seasonal = `decompose_ts_other$seasonal`)
atl <- atl %>% 
  mutate(s = 1)
other <- other %>% 
  mutate(s = 1)

ggplot() +
  geom_line(atl, mapping = aes(month, seasonal, group = s), color = "red")+
  geom_line(other, mapping = aes(month, seasonal, group = s))
