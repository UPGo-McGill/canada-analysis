library(tidyverse)
library(lubridate)

#creations
created <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31") %>% 
  mutate(day = yday(Created)) %>% 
  group_by(Property_ID, day) %>% 
  summarise(n = n()) %>% 
  group_by(day) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n))

created_cmaname <- Canada_daily %>% 
  filter(CMATYPE== "CMA",Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, CMAgroup, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMAgroup, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMAgroup) %>% 
  mutate(percent = n/sum(n))

month3 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
created_freh$month2 <- factor(created_freh$month2, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
created_freh$FREH2 <- coalesce(created_freh$FREH2, c("Part-time"))
created_freh$FREH2 <- factor(created_freh$FREH2, levels = c("Part-time", "FREH", "VFREH"))
three_color <- c("#8c6bb1", "#9ebcda", "#0c316b")

ggplot(data = created_freh)+
  geom_line(mapping = aes(month2, percent, group = FREH2, color = FREH2))+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=8),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size =8),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_y_continuous(labels = scales::percent_format())+
  scale_colour_manual(values = three_color, name = "")+
  xlab("Month")+
  ylab("Percent of creations")


created_freh <- created_freh %>% 
  mutate(month2 = month3)
created_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31", Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2) %>% 
  mutate(percent = n/sum(n))

created_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, ML, month) %>% 
  summarise(n = n()) %>% 
  group_by(ML, month) %>% 
  summarise(n = n()) %>% 
  group_by(ML) %>% 
  mutate(percent = n/sum(n))

## rampup period
r_diff<- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(x = day_num - day_num_r) %>% 
  filter(x <= 180) %>% 
  summarise(x = mean(x, na.rm = TRUE))

mean(r_diff$x)

#average version
r_diff2_freh <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", Listing_Type == 'Entire home/apt', day_num_r == 1) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, FREH7) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(FREH7) %>% 
  summarise(mean(x, na.rm = TRUE))

#graph version
r_diff2_freh2 <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", Listing_Type == 'Entire home/apt', day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, FREH3) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, FREH3) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_cmagroup <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, CMAgroup) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, CMAgroup) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_L_type <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month,Listing_Type) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, Listing_Type) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_ml <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month,ML) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, ML) %>% 
  summarise(mean(x, na.rm = TRUE))

ggplot(data = r_diff2_ml, mapping = aes(x = month, y = `mean(x, na.rm = TRUE)`, color = MLnew, group = MLnew))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Duration of ramp-up period")+
  xlab("Month")

##PPN--------------------------------------------------------------------------

#Overall
ppnmed_overall <-Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

#CMATYPE
ppnmed_cmatype <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, CMATYPE) %>% 
  summarise(ppn = median(Price), n = n())

#CMATYPE average
ppnmed_cmatype <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(CMATYPE) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

#FREH
ppnmed_freh <- Canada_daily %>% 
  filter(Status == "R", day_num_r <= 730, Created >= "2016-09-01", Listing_Type == "Entire home/apt", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, FREH7) %>% 
  summarise(ppn = median(Price), n = n())

#FREH avg
ppnmed_freh <- Canada_daily %>% 
  filter(Status == "R", day_num_r <= 730, Created >= "2016-09-01", Listing_Type == "Entire home/apt", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(FREH2) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

#CMANAME
ppnmed_cmaname <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, CMATYPE == "CMA") %>% 
  group_by(day_num_r, CMAgroup) %>% 
  summarise(ppn = median(Price), n = n())

#CMANAME average
ppnmed_cmaname <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, CMATYPE == "CMA") %>% 
  group_by(CMAgroup) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

#LT
ppnmed_lt <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, Listing_Type) %>% 
  summarise(ppn = median(Price), n = n())

#LT average
ppnmed_lt <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(Listing_Type) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

#ML
ppnmed_ml <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r,MLnew) %>% 
  summarise(ppn = median(Price), n = n())

#ML average
ppnmed_ml_avg <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(MLnew) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

#month of creation
ppnmed_mon <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  mutate(month = format(as.Date(Created), "%m")) %>%
  group_by(day_num_r, month) %>% 
  summarise(ppn = median(Price), n = n())


#month/year of creation
ppnmed_monyr <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>%
  group_by(day_num_r, month) %>% 
  summarise(ppn = median(Price), n = n())


ggplot(ppnmed_ml, aes(x = day_num_r, y = ppn))+
  geom_line(aes(color = MLnew))+
  geom_smooth(aes(color = MLnew))
  xlim(0,730)
  facet_wrap(~CMANAME)

ppnmed_cmaname <- ppnmed_cmaname %>% 
  filter(ppn < 150)

#occ rate-----------------------------------------------------------------------------------

occ <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  ungroup() %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_overall <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMATYPE, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

#average
occ_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMATYPE) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMAgroup, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_lt <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Listing_Type, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

#lt average
occ_lt <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Listing_Type) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(MLnew)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  group_by(ML2, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

#average ml
occ_ml_avg <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(MLnew) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Listing_Type == "Entire home/apt", Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(FREH7, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

#average freh
occ_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Listing_Type == "Entire home/apt", Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(FREH2) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

ggplot(occ_ml, aes(day_num_r, occrate))+
  geom_line(aes(group = ML2, color = ML2))+
  xlim(0,730)
facet_wrap(~CMATYPE)


### Percent with res -------------------------------------------------------
ggplot(perc_res_ml, aes(MLnew, value))+
  geom_bar(stat = "identity", position = "stack", aes(fill = key))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4))


perc_res <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>% 
  group_by(Property_ID) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  summarise(withres = sum(res)/n(), 
            without_res = (n() - sum(res))/n())
              
           

perc_res_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>% 
  group_by(Property_ID, CMAgroup) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(CMAgroup) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

perc_res_freh <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID, FREH2) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(FREH2) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

perc_res_lt <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>% 
  group_by(Property_ID, Listing_Type) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(Listing_Type) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")


perc_res_cmatype <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(CMATYPE) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

perc_res_mon <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>%
  mutate(month = format(as.Date(Created), "%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(month) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

perc_res_yearmon <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-01") %>%
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(month) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

# Reservation length -----------------------------------------------------------------

#CMATYPE
res_length_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(CMATYPE, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(CMATYPE) %>% 
  summarise(mean(n))

ggplot(res_length_ml, aes(MLnew, `mean(n)`)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#CMANAME
res_length_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(CMAgroup, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(CMAgroup) %>% 
  summarise(mean(n))

res_length_cmaname <- res_length %>% 
  left_join(popCMA, by = "CMANAME")

ggplot(res_length_freh, aes(FREH3, `mean`)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#FREH
res_length_freh <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", Listing_Type == "Entire home/apt") %>% 
  group_by(FREH7, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(FREH7) %>% 
  summarise(mean = mean(n), median = median(n))


#ML
res_length_lt <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(Listing_Type, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(Listing_Type) %>% 
  summarise(mean(n))

#mon
res_length_mon <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01") %>%
  mutate(month = format(as.Date(Created), "%m")) %>% 
  group_by(month, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(month) %>% 
  summarise(mean(n))

#mon
res_length_yearmon <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01") %>%
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  group_by(month, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(month) %>% 
  summarise(mean(n))

#monres
res_length_monres <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01") %>%
  group_by(Date, Reservation) %>% 
  summarise(n = n()) %>% 
  mutate(month = format(as.Date(Date), "%Y-%m")) %>% 
  group_by(month, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n<30) %>% 
  group_by(month) %>% 
  summarise(mean(n))

#day_num
res_length_dayres <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01") %>%
  group_by(Reservation) %>% 
  summarise(n = n(), median = median(day_num_r)) %>% 
  filter(n<30) %>%
  group_by(median) %>% 
  summarise(mean(n))

#winddown period-------------------------------------------------------------------------


winddown_cmatype <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  group_by(Property_ID, CMATYPE) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(CMATYPE) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

winddown_cmaname <- Canada_daily %>% 
  filter(Housing == TRUE, CMATYPE == "CMA", Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  group_by(Property_ID, CMAgroup) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(CMAgroup) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))


winddown_lt <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  group_by(Property_ID, Listing_Type) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(Listing_Type) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

winddown_freh <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25", Listing_Type == "Entire home/apt") %>%
  group_by(Property_ID, FREH7) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(FREH7) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

winddown_mon <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  mutate(month = format(as.Date(Created), "%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(month) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

winddown_mon_yr <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(month) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))


winddown_overall <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2018-01-25") %>%
  group_by(Property_ID) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE), n = n())

winddown_overall2 <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Scraped <= "2018-01-25") %>%
  group_by(Property_ID) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE), n = n())

ggplot(winddown_ml, aes(MLnew, deathdiff))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Death

dead_overall <- Canada_daily %>% 
  filter(Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01", !(is.na(CMATYPE))) %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(Property_ID, month) %>% 
  summarise(n = n()) %>% 
  group_by(month)
summarise(percent = n/sum(n))

dead_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(CMAgroup, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMAgroup) %>% 
  mutate(percent = n/sum(n))

dead_cmatype <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(CMATYPE, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>% 
  mutate(percent = n/sum(n))

dead_lt <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(Listing_Type, month) %>% 
  summarise(n = n()) %>% 
  group_by(Listing_Type) %>% 
  mutate(percent = n/sum(n))

dead_freh <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01", Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(FREH7, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH7) %>% 
  mutate(percent = n/sum(n))

dead_mon <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Scraped)) %>% 
  mutate(month2 = month(Created)) %>% 
  group_by(month2, month) %>% 
  summarise(n = n()) %>% 
  group_by(month2) %>% 
  mutate(percent = n/sum(n))

dead_mon_yr <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Scraped)) %>% 
  mutate(month2 = format(as.Date(Date), "%Y-%m")) %>%  
  group_by(month2, month) %>% 
  summarise(n = n()) %>% 
  group_by(month2) %>% 
  mutate(percent = n/sum(n))

ggplot(dead_ml, aes(month, percent))+
  geom_line(aes(color = MLnew))
  facet_wrap(~CMANAME)


dead_mon_yr2 <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = format(as.Date(Scraped), "%Y-%m")) %>%  
  group_by(CMATYPE, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>% 
  mutate(percent = n/sum(n))

#How much revenue do listings earn

rev_mon <- rev_mon %>% 
  filter(rev<25000)
rev_mon$month <- as.factor(rev_mon$month)
ggplot(rev_mon, aes(month, rev))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rev_cmatype2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(CMATYPE) %>% 
  summarise(rev = median(rev), revCAD = rev*1.2957)

rev_cmaname2 <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  group_by(Property_ID, CMAgroup) %>% 
  summarise(rev = sum(Price))%>% 
  group_by(CMAgroup) %>% 
  summarise(rev = median(rev), revCAD = rev*1.2957)

rev_LT2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  group_by(Property_ID, Listing_Type) %>% 
  summarise(rev = sum(Price))%>% 
  group_by(Listing_Type) %>% 
  summarise(rev = median(rev), revCAD = rev*1.2957)

rev_freh2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID, FREH2) %>% 
  summarise(rev = sum(Price))%>% 
  group_by(FREH2) %>% 
  summarise(rev = median(rev), revCAD = rev*1.2957)

rev_ml2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  group_by(Property_ID, MLnew) %>% 
  summarise(rev = sum(Price))%>% 
  group_by(MLnew) %>% 
  summarise(rev = median(rev), revCAD = rev*1.2957)

rev_mon2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(Property_ID, month) %>% 
  summarise(rev = sum(Price))%>% 
  group_by(month) %>% 
  summarise(median(rev))

rev <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Status == "R", Scraped <= "2019-01-25") %>% 
  group_by(Property_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  summarise(rev = median(rev), revCAD = rev*1.2957)



# life length -----------------------------------------------------------------------------

length_cmatype <- Canada_daily %>%
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25") %>% 
  group_by(CMATYPE) %>% 
  summarise(life = median(lifelength), life2 = mean(lifelength))

length_cmaname <- Canada_daily %>%
  filter(CMATYPE == "CMA", Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25") %>% 
  group_by(CMAgroup) %>% 
  summarise(life = median(lifelength), life2 = mean(lifelength))

length_freh <- Canada_daily %>%
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25", Listing_Type == "Entire home/apt") %>% 
  group_by(FREH3) %>% 
  summarise(life = median(lifelength), life2 = mean(lifelength))

length_freh2 <- Canada_daily %>%
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25", Listing_Type == "Entire home/apt") %>% 
  group_by(FREH2) %>% 
  summarise(life = median(Scraped - Created), life2 = mean(Scraped - Created))

length_lt <- Canada_daily %>%
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25") %>% 
  group_by(Listing_Type) %>% 
  summarise(life = median(lifelength), life2 = mean(lifelength))

length_mon <- Canada_daily %>%
  filter(!is.na(CMATYPE), Housing == TRUE, Created>="2016-09-01", Scraped <= "2019-01-25") %>% 
  mutate(created = format(as.Date(Created), "%m")) %>%
  group_by(created) %>% 
  summarise(life = median(lifelength), life2 = mean(lifelength))

## are listings dying over time living longer

dd <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Scraped <= "2019-01-25") %>% 
  mutate(length = Scraped - Created,
         scraped = format(as.Date(Scraped), "%Y-%m")) %>%
  group_by(Property_ID, scraped, length) %>% 
  summarise(n()) %>% 
  group_by(scraped) %>% 
  summarise(median(length))

ggplot(dd, aes(x = scraped, y = `median(length)`))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

## yoy difference in ramp up
#only includes listings that got a reservation at some point
ramp2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, day_num_r == 1, Created <= "2018-08-31") %>% 
  mutate(year = ifelse(Created >= "2017-09-01" & Created <= "2018-08-31", "LTM",
                       ifelse(Created >= "2016-09-01" & Created <= "2017-03-31", "MTM", NA))) %>%
  mutate(x = day_num - day_num_r) %>% 
  group_by(year) %>% 
  summarise(x = mean(x, na.rm = TRUE))

r_diff<- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(x = day_num - day_num_r) %>% 
  filter(x <= 180) %>% 
  summarise(x = mean(x, na.rm = TRUE))


  
## yoy difference in ppn

price <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01", Status == "R") %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(year) %>% 
  summarise(ppn = median(Price))

price2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01", Status == "R") %>% 
  mutate(year = ifelse(Date >= "2018-01-29" & Date <= "2019-01-28", "LTM",
                       ifelse(Date >= "2017-01-29" & Date <= "2018-01-28", "MTM", NA))) %>% 
  group_by(year, day_num_r) %>% 
  summarise(ppn = median(Price))

price3 <- spread(price2, key = year, value = ppn)
price3 <- price3 %>% 
  mutate(growth = (LTM-MTM)/MTM)

price3 %>% 
  filter(day_num_r<=365) %>% 
  summarise(med = mean(growth, na.rm = TRUE))

price4 <- price2 %>% 
  filter(day_num_r<=365, !(is.na(year)))

ggplot(data = price4, aes(x = day_num_r, y = ppn, color = year))+
  geom_line()+
  geom_smooth()


##yoy difference in wind down

winddown_mon_yr2 <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Scraped <= "2018-01-25") %>%
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(month) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

wind2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Scraped <= "2019-01-25") %>% 
  mutate(month = format(as.Date(Scraped), "%Y-%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(month) %>% 
  summarise(x = mean(deathdiff, na.rm = TRUE))

ggplot(data = wind, aes(x = month, y = x))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

r_diff<- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Created <= "2018-08-01", Scraped <= "2019-01-25", day_num_r == 1) %>% 
  mutate(x = day_num - day_num_r) %>% 
  filter(x <= 180) %>% 
  summarise(x = mean(x, na.rm = TRUE))
