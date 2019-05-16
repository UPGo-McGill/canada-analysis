# --------------------- Extra stuff to add to the daily file ---------------- #
Canada_daily <- Canada_daily %>% 
  arrange(Property_ID, Date) %>%
  group_by(Property_ID) %>%
  mutate(day_num = row_number()) %>% # days since created
  mutate(day_num_r = ifelse(row_number() - match("R", Status) >= 0, row_number() - match("R", Status) + 1, NA)) %>% # days since first reservation
  mutate(rev = ifelse(Status == "R", Price, 0)) %>% # revenue - that day
  mutate(cumrev = cumsum(rev))%>% # cumulative revenue
  mutate(lifelength = n()) # length of existence

Canada_daily_seas <- Canada_daily_seas %>% 
  mutate(FREH3 = ifelse(Listing_Type == "Entire home/apt" & R3652 >= 120 & R3652 + A3652 >=240 , "VFREH", 
                       ifelse(Listing_Type == "Entire home/apt" & R3652 >= 60 & R3652 + A3652 >=120, "FREH", "Part-time")))

Canada_daily_seas <- Canada_daily_seas %>% 
  group_by(Property_ID) %>% 
  mutate(FREH4 = ifelse(sum(Canada_daily_seas$FREH3 == "VFREH")>0, "VFREH",
                        ifelse(sum(Canada_daily_seas$FREH3 == "FREH")>0, "FREH", "Part-time")))

Canada_daily <- Canada_daily %>% 
  mutate(FREH6 = ifelse(Listing_Type == "Entire home/apt" & R3652 >= 150 & R3652 + A3652 >=300 , "A",
                        ifelse(Listing_Type == "Entire home/apt" & R3652 >= 135 & R3652 + A3652 >=270 , "B",
                               ifelse(Listing_Type == "Entire home/apt" & R3652 >= 120 & R3652 + A3652 >=240 , "C", 
                                      ifelse(Listing_Type == "Entire home/apt" & R3652 >= 105 & R3652 + A3652 >=210, "D", 
                                             ifelse(Listing_Type == "Entire home/apt" & R3652 >= 90 & R3652 + A3652 >=183, "E", 
                                                    ifelse(Listing_Type == "Entire home/apt" & R3652 >= 75 & R3652 + A3652 >=150, "F", 
                                                           ifelse(Listing_Type == "Entire home/apt" & R3652 >= 60 & R3652 + A3652 >=120, "G","Part-time"))))))))

c("Booked >= 150 nights/available >= 300 nights","Booked >= 135 nights/available >= 270 nights",
  "Booked >= 120 nights/available 240 nights","Booked >= 105 nights/available 210 nights",
  "Booked >= 90 nights/available 183 nights","Booked >= 75 nights/available 150 nights",
  "Booked >= 60 nights/available 120 nights","Part-time")

big <- c("Toronto", "Montréal", "Vancouver", "Calgary", "Edmonton", "Ottawa - Gatineau", "Québec", "Winnipeg", "Hamilton", "Kitchener - Cambridge - Waterloo")
mid <- c("London", "Oshawa", "Windsor", "Saskatoon", "Regina", "Sherbrooke", "Barrie", "Abbotsford - Mission", "Sudbury", "Kingston")
small <- c("Brantford", "Peterborough", "Thunder Bay", "Lethbridge", "Belleville", "Saguenay", "Trois-Rivières", "Guelph")
tourist <- c("St. Catharines - Niagara", "Halifax", "Victoria", "Kelowna", "Saint John", "St. John's", "Moncton")

Canada_daily <- Canada_daily %>% 
  mutate(CMAgroup = ifelse(CMANAME %in% big, "big",
                           ifelse(CMANAME %in% mid, "mid", 
                                  ifelse(CMANAME %in% small, "small", 
                                         ifelse(CMANAME %in% tourist, "tourist", NA)))))

A <- Canada_daily %>% 
  filter(FREH6 == "A") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(A = FREH6)

B <- Canada_daily %>% 
  filter(FREH6 == "B") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(B = FREH6)

C <- Canada_daily %>% 
  filter(FREH6 == "C") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(C = FREH6)

D <- Canada_daily %>% 
  filter(FREH6 == "D") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(D = FREH6)

E <- Canada_daily %>% 
  filter(FREH6 == "E") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(E = FREH6)

F1 <- Canada_daily %>% 
  filter(FREH6 == "F") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(F1 = FREH6)

G <- Canada_daily %>% 
  filter(FREH6 == "G") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename(G = FREH6)

PT <- Canada_daily %>% 
  filter(FREH6 == "Part-time") %>% 
  group_by(Property_ID, FREH6) %>% 
  summarise(n = n()) %>% 
  rename("Part-time" = FREH6)

H <- PT %>% 
  left_join(A[,c(1:2)], by = "Property_ID") %>% 
  left_join(B[,c(1:2)], by = "Property_ID") %>% 
  left_join(C[,c(1:2)], by = "Property_ID") %>% 
  left_join(D[,c(1:2)], by = "Property_ID") %>% 
  left_join(E[,c(1:2)], by = "Property_ID") %>% 
  left_join(F1[,c(1:2)], by = "Property_ID") %>% 
  left_join(G[,c(1:2)], by = "Property_ID")

H <- H %>% 
  mutate(FREH8 = ifelse(!is.na(A), A, 
                        ifelse(!is.na(B), B, 
                               ifelse(!is.na(C), C,
                                      ifelse(!is.na(D), D, 
                                             ifelse(!is.na(E), E,
                                                    ifelse(!is.na(F1), F1, 
                                                           ifelse(!is.na(G), G, "Part-time"))))))))


Canada_daily <- Canada_daily %>% 
  left_join(H[c(1,11)], by = "Property_ID")

test4 <- Canada_daily %>%
  filter(Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID, FREH7) %>% 
  summarise(n = n()) %>% 
  group_by(FREH7) %>% 
  summarise(n = n())

test3 <- Canada_property %>% 
  filter(!Property_ID %in% test$Property_ID)

colnames(Canada_daily)
Canada_daily <- Canada_daily %>% 
  select(-FREH3, -FREH4, -FREH5, -FREH6)

library(tidyverse)
# --------------- Sample of 1,000 listings revenue over life ---------------- #
## ------------------------- Colored Line Charts ---------------------------- #

Canada_property_oct16 <- Canada_property %>% 
  filter(Created >= "2016-09-01")
subset <- sample(Canada_property_oct16$Property_ID, size = 1000, replace = FALSE)
subset_daily <- Canada_daily %>% 
  filter(Property_ID %in% subset)

# color by length of listing
ggplot(subset_daily, aes(x = day_num, y = cumrev))+
  geom_line(aes(group = Property_ID, color = lifelength), alpha = 0.2)+
  scale_color_gradientn(colors = rainbow(100))

# color by ML status
ggplot(subset_daily, aes(x = day_num, y = cumrev))+
  geom_line(aes(group = Property_ID, color = ML), alpha = 0.2)+
  geom_line(data = teste, aes(x = day_num, y = med_rev, group = ML, color = ML))
  
# color by FREH status
ggplot(subset_daily, aes(x = day_num, y = cumrev))+
  geom_line(aes(group = Property_ID, color = FREH), alpha = 0.2)
  geom_line(data = testc, aes(x = day_num, y = med_rev, group = FREH, color = FREH))
ggplot(data = testc, aes(x = day_num, y = med_rev))
  geom_line(aes(group = FREH, color = FREH))


# ------------------ How does life length vary by CMA/Geography --------------- #
# -------------------------- Boxplots and Violin Charts ----------------------- #

# life length by CMATYPE - VIOLIN CHART
boxplot_cmatype <- Canada_daily %>% 
  group_by(Property_ID) %>% 
  filter(sum(Status == "R") > 0) %>% 
  ungroup() %>% 
  filter(!is.na(CMATYPE), Created>"2016-10-01") %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(lifelength = mean(lifelength))

ggplot(data = boxplot_cmatype, aes(x = CMATYPE, y = lifelength))+
  geom_violin()

# life length by CMA
boxplot_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(Property_ID) %>% 
  filter(sum(Status == "R") > 0) %>% 
  ungroup() %>% 
  group_by(Property_ID, CMANAME) %>% 
  summarise(lifelength = mean(lifelength))

ggplot(data = boxplot_cmaname, aes(x = CMANAME, y = lifelength))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
# --------------- Days since first res vs. median cumulative revenue --------------------#

#CMATYPE
medcumrev_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(CMATYPE, day_num_r) %>% 
  summarise(med_rev = median(cumrev))

ggplot(data = medcumrev_cmatype, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = CMATYPE, color = CMATYPE))
  facet_wrap(~CMATYPE)

#CMANAME
medcumrev_cmaname <- Canada_daily %>% 
  filter(!is.na(CMANAME)) %>% 
  group_by(CMANAME, day_num_r) %>% 
  summarise(med_rev = median(cumrev))

ggplot(data = medcumrev_cmaname, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = CMANAME, color = CMANAME), show.legend = FALSE)+
  facet_wrap(~CMANAME)

#ML
medcumrev_ml <- Canada_daily %>% 
  group_by(ML, day_num_r) %>% 
  summarise(med_rev = median(cumrev))

ggplot(data = medcumrev_ml, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = ML, color = ML))
  facet_wrap(~ML)

#FREH
medcumrev_freh <- Canada_daily %>% 
  group_by(FREH, day_num_r) %>% 
  summarise(med_rev = median(cumrev))

ggplot(data = medcumrev_freh, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH))
  facet_wrap(~FREH)

#Listing Type
medcumrev_LT <- Canada_daily %>% 
  group_by(Listing_Type, day_num_r) %>% 
  summarise(med_rev = median(cumrev))
  
ggplot(data = medcumrev_LT, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = Listing_Type, color = Listing_Type))
  facet_wrap(~Listig_Type)

#FREH and CMANAME
medcumrev_freh_CMANAME <- Canada_daily %>% 
  group_by(CMANAME, FREH, day_num_r) %>% 
  summarise(med_rev = median(cumrev))
  
ggplot(data = medcumrev_freh_CMANAME, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH)) +
  facet_wrap(~CMANAME)

#ML and CMANAME
medcumrev_ml_CMANAME <- Canada_daily %>% 
  group_by(CMANAME, ML, day_num_r) %>% 
  summarise(med_rev = median(cumrev))

ggplot(data = medcumrev_ml_CMANAME, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = ML, color = ML)) +
  facet_wrap(~CMANAME)
  
# --------------------- How fast are listings making money ----------------------------- #
# ---------------------- based on when they were created ------------------------------- #

prop_spec <- Canada_property %>% 
  filter(Created >= "2016-10-01") %>% 
  mutate(yearmon = yearmonth(Created))

year <- Canada_daily %>% 
  right_join(prop_spec[,c(1,31)], by = "Property_ID")


yearvyear3 <- Canada_daily %>% 
  filter(Created >= "2016-10-01", Housing == TRUE) %>% 
  mutate(yearmon = yearmonth(Created))
  
year2 <- year %>% 
  group_by(yearmon, day_num) %>% 
  summarise(med_rev = median(cumrev))
year3 <- year2 %>% 
  group_by(yearmon) %>% 
  slice(1:(n()-20))

slice(test_df, 1:(n()-5))

ggplot(data = year3, aes(x = day_num, y = med_rev))+
  geom_line(aes(group = yearmon, color = yearmon))
  facet_wrap(~yearmon)
  scale_color_continuous(rainbow(28))



# ----------------------------- Scatterplot ------------------------------------------- #

scatter <- Canada_daily %>% 
  filter(day_num_r>0, Created > "2017-12-31") %>% 
  group_by(Property_ID) %>% 
  filter(day_num_r == max(day_num_r))


ggplot(data = scatter, aes(x = day_num_r, y = cumrev))+
  geom_point(aes(color = CMANAME), show.legend = FALSE)+
  geom_smooth()+
  facet_wrap(~CMANAME)

# -------------------------- Density plots of life length -----------------------------#
# --------------------- How much time between first and last r ------------------------#

#CMANAME
R_diff_compare <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(Property_ID) %>% 
  mutate(r_diff = ifelse(sum(Status == "R") > 0,  
                         which.max(day_num[Status == "R"]) - which.min(day_num[Status == "R"]) + 1, NA)) %>% 
  group_by(Property_ID, CMANAME) %>% 
  summarise(median(r_diff, na.rm = TRUE), median(lifelength))

R_diff_compare <- R_diff_compare %>% 
  gather(`median(r_diff)`, `median(lifelength)`, key = "key", value = "value")

ggplot(data = R_diff_compare)+
  geom_density(aes(value, fill = key),alpha = 0.4)+
  facet_wrap(~CMANAME)

#CMATYPE
R_diff_compare_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(Property_ID) %>% 
  mutate(r_diff = ifelse(sum(Status == "R") > 0,
                         which.max(day_num[Status == "R"]) - which.min(day_num[Status == "R"]) + 1, 0)) %>% 
  group_by(Property_ID, CMATYPE) %>% 
  summarise(median(r_diff), median(lifelength))

R_diff_compare_cmatype <- R_diff_compare_cmatype %>% 
  gather(`median(r_diff)`, `median(lifelength)`, key = "key", value = "value")

ggplot(data = R_diff_compare_cmatype)+
  geom_density(aes(value, fill = key),alpha = 0.4)+
  facet_wrap(~CMATYPE)
  
# ------------------------------ Occupancy Rates by Date -------------------------------#
#CMANAME 
occ_cmaname_date <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(CMANAME, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n())

ggplot(occ_cmaname_date, aes(day_num_r, occrate))+
  geom_line(aes(group = CMANAME, color = CMANAME), show.legend = FALSE) +
  facet_wrap(~CMANAME)

#CMATYPE

occ_overall <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMATYPE, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMANAME, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_lt <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Listing_Type, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(ML)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  group_by(ML, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(FREH2, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_mon <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  mutate(month = format(as.Date(Created), "%m")) %>%
  group_by(month, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

ggplot(occ_mon, aes(day_num_r, occrate))+
  geom_line(aes(group = month, color = month))
  facet_wrap(~CMANAME)

# ----------------------- What % of listings ever get a reservation --------------------#

perc_res_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME) %>% 
  summarise(res = ifelse(sum(Status == "R") > 0, 1, 0)) %>% 
  group_by(CMANAME) %>% 
  summarise(withres = sum(res)/n(), without_res = (n() - sum(res))/n()) %>% 
  gather(without_res, withres, key = "key", value = "value")

ggplot(perc_res, aes(CMANAME, value))+
  geom_bar(stat = "identity", position = "stack", aes(fill = key))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4))


# ------------------ What type of listings are created when ---------------------------#
type <- Canada_daily %>% 
  mutate(month = month(Created)) %>% 
  group_by(month) %>% 
  summarise(shared = sum(Listing_Type == "Shared room")/n(), 
            private = sum(Listing_Type == "Private room")/n(),
            entire = sum(Listing_Type == "Entire home/apt")/n()) %>% 
  gather(shared, private, entire, key = "key", value = "value")

ggplot(type, aes(month, value))+
  geom_bar(stat = "identity", position = "stack", aes(fill = key))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ----------------------- When are listings created ----------------------------------#

created3 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Date >= "2016-09-01", Date <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, month) %>% 
  summarise(n = n()) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n))

created_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Date >= "2016-09-01", Date <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, CMATYPE, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>% 
  mutate(percent = n/sum(n))

largest <- Canada_daily %>%
  filter(CMATYPE == "CMA", Housing == TRUE, Date >= "2016-09-01", Date <= "2018-08-31") %>% 
  group_by(Date, CMANAME) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME) %>% 
  summarise(mean(n))

largest <- largest %>% 
  arrange(-`mean(n)`) %>% 
  mutate(size = row_number())

created_cmaname <- created_cmaname %>% 
  left_join(largest, by = "CMANAME")

created_cmaname <- Canada_daily %>% 
  filter(CMATYPE== "CMA",Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, CMAgroup, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMAgroup, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMAgroup) %>% 
  mutate(percent = n/sum(n))

library(lubridate)

created_freh <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", Created <= "2018-08-31", Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, FREH2, month) %>%
  summarise(n = n()) %>% 
  group_by(FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2) %>% 
  mutate(percent = n/sum(n))

created_freh2 <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", Scraped <= "2019-01-25", Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, FREH2, month) %>%
  summarise(n = n()) %>% 
  group_by(FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2) %>% 
  mutate(percent = n/sum(n))

created_freh3 <- Canada_daily %>% 
  filter(Housing == TRUE, Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, FREH2, month) %>%
  summarise(n = n()) %>% 
  group_by(FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2) %>% 
  mutate(percent = n/sum(n))

created_ml <- Canada_daily %>% 
  filter(Housing == TRUE, Date >= "2016-09-01", Date <= "2018-08-31") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(ML)>0, TRUE, FALSE)) %>% 
  group_by(Property_ID, ML2, month) %>% 
  summarise(n = n()) %>% 
  group_by(ML2, month) %>% 
  summarise(n = n()) %>% 
  group_by(ML2) %>% 
  mutate(percent = n/sum(n))


  
ggplot()+
  geom_line(created_freh3, mapping = aes(month, percent, color = FREH2))
  facet_wrap(~size)
  geom_line(Fig4, mapping = aes(month2, percrev), color = "orange")

  
r_diff <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01") %>%
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month) %>% 
  summarise(mean(x, na.rm = TRUE))
r_diff2 <- r_diff %>% 
  filter(!month %in% c("2019-01", "2018-12", "2018-11", "2018-10", "2018-09", "2018-08"))

ggplot(data = r_diff2, mapping = aes(x = month, y = `mean(x, na.rm = TRUE)`))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Month")+
  ylab("Ramp-up period duration")

r_diff2_freh <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2019-01-25", Listing_Type == 'Entire home/apt', day_num_r == 1) %>% 
  group_by(Property_ID, FREH2) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  filter(diff <= 180) %>% 
  group_by(FREH2) %>% 
  summarise(diff = mean(diff, na.rm = TRUE))

r_diff2_freh2 <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01", Scraped <= "2019-01-25", Listing_Type == 'Entire home/apt', day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, FREH2) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, FREH2) %>% 
  summarise(mean(x, na.rm = TRUE))


r_diff2_ml <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01") %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(ML, na.rm= TRUE)>0, TRUE, FALSE)) %>% 
  ungroup %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, ML2) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, ML2) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_name <- Canada_daily %>% 
  filter(Housing == TRUE, CMATYPE == "CMA", Created >= "2016-09-01") %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, CMANAME) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, CMANAME) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_type <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01") %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, CMATYPE) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, CMATYPE) %>% 
  summarise(mean(x, na.rm = TRUE))

r_diff2_L_type <- Canada_daily %>% 
  filter(Housing == TRUE, !is.na(CMATYPE), Created >= "2016-09-01") %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month,Listing_Type) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, Listing_Type) %>% 
  summarise(mean(x, na.rm = TRUE))


# ----------------------- When do listings die ---------------------------------------#
  
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
  group_by(CMANAME, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMANAME) %>% 
  mutate(percent = n/sum(n))

ggplot(dead_cmaname, aes(month, percent))+
  geom_line(aes(color = CMANAME))+
  facet_wrap(~CMANAME)


# ------------------------ Average length of one reservation -------------------------#

#CMATYPE
res_length_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(CMATYPE, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(CMATYPE) %>% 
  summarise(mean(n))
  
ggplot(res_length, aes(CMATYPE, `mean(n)`)) +
  geom_col()
  
#CMANAME
res_length_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(CMANAME, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(CMANAME) %>% 
  summarise(mean(n))

res_length_cmaname <- res_length %>% 
  left_join(popCMA, by = "CMANAME")

ggplot(res_length_cmaname, aes(fct_reorder(CMANAME, `mean(n)`), `mean(n)`)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#FREH
res_length_freh <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01") %>% 
  group_by(FREH2, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(FREH2) %>% 
  summarise(mean(n))


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


#------------------------- Speed between ML acquisitions ----------------------#
ml <- Canada_daily %>% 
  filter(ML == TRUE, Housing == TRUE) %>% 
  group_by(Property_ID) %>%
  summarise(n = n(), Created = mean(Created), Host_ID = mean(Host_ID)) %>% 
  group_by(Host_ID) %>% 
  arrange(Created, .by_group = TRUE) %>% 
  mutate(time_diff = Created - lag(Created, 1), id = row_number())

ml$Host_ID <- as.factor(ml$Host_ID)


ggplot(ml, aes(id, time_diff))+
  geom_point(aes(group = Host_ID, color = Host_ID), alpha = 0.5, show.legend = FALSE)+
  geom_line(data= ml2, aes(x = id, y = g))
  
ml2 <- ml %>% 
  group_by(id) %>% 
  summarise(g = mean(time_diff))


# -------------------- Differences in Price per night -----------------------#

ggplot(ppnmed_overall2, aes(x = day_num_r, y = ppn))+
  geom_point()+
  geom_smooth()
  facet_wrap(~CMANAME)
  
ppnmed_overall2 <- ppnmed_overall2 %>% 
  filter(ppn < 100)

#CMATYPE
ppnmed_cmatype <- as %>% 
  filter(Status == "R",Created >= "2016-10-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, CMATYPE) %>% 
  summarise(ppn = median(Price), n = n())

#FREH
ppnmed_freh <- Canada_daily %>% 
  filter(Status == "R", Created >= "2016-10-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, FREH2) %>% 
  summarise(ppn = median(Price), n = n())

#ML
ppnmed_ml <- Canada_daily %>% 
  filter(Status == "R", Created >= "2016-10-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(ML)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  group_by(day_num_r, ML2) %>% 
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




counts2 <- Canada_daily %>% 
  filter(Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(Property_ID, CMATYPE, Listing_Type,FREH2) %>% 
  summarise(n()) %>% 
  group_by(CMATYPE, Listing_Type, FREH2) %>% 
  summarise(n())

