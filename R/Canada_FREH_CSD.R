Canada_FREH <- read_csv("data/Canada_FREH.csv")

Canada_FREH <- Canada_FREH %>% 
  left_join(Canada_property[,c(1,5,19,20,21,22,24,25,26,30)])

Canada_FREH <- Canada_FREH %>% 
  left_join(Canada_property[,c(1,30)], by = "Property_ID")

FREH_CSD <- Canada_FREH %>% 
  filter(Housing == TRUE) %>% 
  group_by(CMATYPE, CMANAME, CSDNAME, Date) %>% 
  summarise(n = sum(FREH))

FREH_CSD <- FREH_CSD %>% 
  select(Date, CMATYPE, CMANAME, CSDNAME, n)

write_csv(FREH_CSD, "Canada_FREH.csv")


test <- Canada_FREH %>% 
  filter(Date == "2019-01-28", FREH == 1)


FREH_CSD2 %>%
  ungroup() %>% 
  select(Date, CMANAME, n) %>%
  group_by(Date) %>%
  summarise(total = sum(n)) %>%
  filter(Date == ymd("2019-01-28"))

FREH_CSD %>%
  ungroup() %>% 
  select(Date, CMANAME, n) %>%
  group_by(Date) %>%
  summarise(total = sum(n)) %>% 
  ggplot()+
  geom_col(aes(x = Date, y = total))

