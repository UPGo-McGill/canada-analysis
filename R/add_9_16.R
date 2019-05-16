oldprop <- read_csv("data/Canada_Property_2018-05-17.csv")
test1 <- oldprop %>% 
  filter(`Last Scraped Date`> "2016-08-31" & `Created Date` < "2016-10-01")

test2 <- Canada_property %>% 
  filter(Scraped > "2016-08-31" & Created < "2016-10-01")

test3 <- test2 %>% 
  filter(!Property_ID %in% test1$`Property ID`)

test4 <- test1 %>% 
  filter(!`Property ID` %in% test2$Property_ID)

library(tidyverse)
library(data.table)
property <- read_csv("data/Canada_property.csv")
olddaily <- read_csv("data/Canada_daily_old.csv")
processed_daily <- read_csv("data/Canada_daily_processed.csv")
olddaily <- olddaily %>% 
  filter(Date <="2017-08-31"& Date >= "2016-09-01")
daily <- olddaily

# Daily file prep
daily <- setDT(daily)
daily <- daily[,c(1:3,5,8)]
names(daily) <- c("Property_ID","Date","Status","Price","Reservation")
daily[,c(1,4:5)] <- lapply(daily[,c(1,4:5)], as.integer)

# Confirm there are the same number of listings in prop and daily files
daily <- filter(daily, Property_ID %in% property$Property_ID)

# Merge daily and prop files
daily <- daily %>%
  left_join(property[,c(1:2,5:7,19)], by = "Property_ID")


# Remove out-of-range rows
daily <- 
  daily %>% 
  as_tibble() %>%
  filter(Date >= Created & Date <= Scraped)

# Add columns
daily <-
  daily %>% 
  arrange(Property_ID, Date) %>%
  group_by(Property_ID) %>%
  mutate(
    R365 = as.integer(cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)), # Number of R in last 365 days
    A365 = as.integer(cumsum(Status=="A") - replace_na(lag(cumsum(Status=="A"), 365),0)), # Number of A in last 365 days
    Revenue365 = as.integer(cumsum(as.numeric(Status=="R") * Price) - 
                              replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0))) # Total revenue in last 365 days

#remove old part of new daily file
processed_daily <- processed_daily %>% 
  filter(Date>="2017-09-01")

#add empty ML field
daily$ML <- NA

#join two daily files
daily_complete <- bind_rows(daily, processed_daily)

test <- daily_complete %>% 
  group_by(Date) %>% 
  summarise(n = n())

## Must rerun a365, r365, and rev365 columns after this because August 2017 will be incorrect since the 
## addition of September 2016
## or must rerun some 365 calculation, not sure what right now

ggplot(test, aes(x = Date, y = n))+
  geom_line()
