daily_prop2 <- daily_prop %>%
  group_by(City, Date) %>%
  summarise(n())

names(daily_prop2) <- c("City","Date","Num")

daily_prop2 <- daily_prop2[daily_prop2$City == "Calgary",]

ts <- ts(daily_prop2["Num"], start = c(2014, 10, 01), frequency = 365)
ts
autoplot(ts)
dp <- daily_prop %>%
  as_tbl_time(Date) %>%
  group_by(City)
