test2 <- Canada_daily %>% 
filter(Date == "2016-06-01") %>% 
group_by(Property_ID) %>% 
summarise(n())


sum(prop_old$`Created Date`<="2016-07-01"&prop_old$`Last Scraped Date`>="2016-07-01")
sum(prop_old$`Created Date`<="2016-06-30"&prop_old$`Last Scraped Date`>="2016-06-30")

View(test)

test3 <- daily_old %>% 
  group_by(Date) %>% 
  summarise(n())

ggplot(test3, aes(x = Date, y = `n()`))+
  geom_line()

View(test3)

test4 <- daily %>% 
  group_by(Date) %>% 
  summarise(n())

ggplot(test4, aes(x = Date, y = `n()`))+
  geom_line()
