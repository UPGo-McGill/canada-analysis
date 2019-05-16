FREH <- daily_prop %>%
  filter(Listing_Ty == "Entire home/apt") %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(FREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) >= 60 & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), 365),0)) >= 120, 1, 0)) %>% #FREH status
  mutate(VFREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) >= 120 & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), 365),0)) >= 240, 1, 0)) %>% #VFREH status
  mutate(FREHrev = ifelse(FREH == "1" & Status == "R", Price, 0)) %>% #revenue: is FREH and has a reservation that day
  mutate(FREHrevCum = ifelse(FREH == "1", cumsum(as.numeric(Status == "R") * Price), 0)) %>% #total revenue ever earned by an FREH listing
  mutate(FREHrevCum365 = ifelse(FREH == "1", cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), 365),0),0))

FREH_tidy <- FREH %>%
  filter(FREH == "1") %>%
  group_by(Date, City) %>%
  summarise(n(), sum(FREHrev), sum(FREHrevCum), sum(FREHrevCum365))

VFREH_tidy <- FREH %>%
  filter(VFREH == "1") %>%
  group_by(Date, City) %>%
  summarise(n(), sum(VFREHrev), sum(VFREHrevCum), sum(VFREHrevCum365))



##Function Version - need to add revenue for VFREH

FREH_VFREH <- function(daily_prop, window = 365, FREH_R = 60, FREH_AR = 120, VFREH_R = 120, VFREH_AR = 240) {

FREH <- daily_prop %>%
  filter(Listing_Ty == "Entire home/apt") %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(FREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), window),0)) >= FREH_R & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), window),0)) >= FREH_AR, 1, 0)) %>% #FREH status
  mutate(VFREH = ifelse((cumsum(Status=="R") - replace_na(lag(cumsum(Status=="R"), 365),0)) >= VFREH_R & (cumsum(Status!="B") - replace_na(lag(cumsum(Status!="B"), window),0)) >= VFREH_AR, 1, 0)) %>% #VFREH status
  mutate(FREHrev = ifelse(FREH == "1" & Status == "R", Price, 0)) %>% #revenue: is FREH and has a reservation that day
  mutate(FREHrevCum = ifelse(FREH == "1", cumsum(as.numeric(Status == "R") * Price), 0)) %>% #total revenue ever earned by an FREH listing
  mutate(FREHrevCum365 = ifelse(FREH == "1", cumsum(as.numeric(Status=="R") * Price) - replace_na(lag(cumsum(as.numeric(Status=="R") * Price), window),0),0)) #total revenue earned by a FREH listing in the last 365 days

FREH_tidy <- FREH %>%
  filter(FREH == "1") %>%
  group_by(Date, City) %>%
  summarise(n(), sum(FREHrev), sum(FREHrevCum), sum(FREHrevCum365))
}








##Slower correct way to get FREH/VFREH
#Rcount and AR count
FREH <- daily_prop %>%
  filter(Listing_Ty == "Entire home/apt") %>%
  arrange(Property_I, Date) %>%
  group_by(Property_I) %>% 
  mutate(Rcount = rollapplyr(Status == "R", FUN = sum, width=365, align = "right", partial = TRUE)) %>%
  mutate(ARcount = rollapplyr(Status != "B", FUN = sum, width=365, align = "right", partial = TRUE)) 

#assign a 1 or 0 based on VFREH/FREH status
FREH$FREH <- ifelse(FREH$Rcount >= 60 & FREH$ARcount >= 120, 1, 0)
FREH$VFREH <- ifelse(FREH$Rcount >= 120 & FREH$ARcount >= 240, 1, 0)

#FREH counts per city
FREH_tidy2 <- FREH %>%
  filter(FREH == "1") %>%
  group_by(Date, City) %>%
  summarise(n())

#VFREH counts per city
VFREH_tidy <- FREH %>%
  filter(VFREH=="1") %>%
  group_by(Date, City) %>%
  summarise(n())





##Would be fastest best way, but can't get it to work becaus partial can never = True
FREH <- FREH %>% 
  group_by(Property_I) %>% 
  mutate(Rcount = roll_sum(StatusNumR, n=365, align = "right", partial = FALSE, fill = NA)) %>%
  mutate(ARcount = roll_sum(StatusNumAR, n=365, align = "right", partial = FALSE, fill = NA))





toronto <- FREH_tidy[FREH_tidy$City == "Toronto",]
