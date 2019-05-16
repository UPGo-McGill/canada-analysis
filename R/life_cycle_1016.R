# --------------------- Extra stuff to add to the daily file ---------------- #
Canada_daily <- Canada_daily %>% 
  arrange(Property_ID, Date) %>%
  group_by(Property_ID) %>%
  mutate(day_num = row_number()) %>% # days since created
  mutate(day_num_r = ifelse(row_number() - match("R", Status) >= 0, row_number() - match("R", Status) + 1, NA)) %>% # days since first reservation
  mutate(rev = ifelse(Status == "R", Price, 0)) %>% # revenue - that day
  mutate(cumrev = cumsum(rev))%>% # cumulative revenue
  mutate(lifelength = n()) # length of existence

Canada_daily <- Canada_daily %>% 
  mutate(FREH = ifelse(Listing_Type == "Entire home/apt" & R365 >= 60 & R365 + A365 >=120 , TRUE, FALSE))

# --------------- Sample of 1,000 listings revenue over life ---------------- #
## ------------------------- Colored Line Charts ---------------------------- #

subset <- sample(Canada_property$Property_ID, size = 1000, replace = FALSE)
subset_daily <- Canada_daily %>% 
  filter(Property_ID %in% subset)

# color by length of listing
ggplot(subset_daily, aes(x = day_num, y = cumrev))+
  geom_line(aes(group = Property_ID, color = lifelength), alpha = 0.2)+
  scale_color_gradientn(colors = rainbow(100))

# color by ML status
ggplot(subset_daily, aes(x = day_num, y = cumrev))+
  geom_line(aes(group = Property_ID, color = ML), alpha = 0.2)

# color by FREH status
ggplot(subset_daily, aes(x = day_num, y = cumrev))+
  geom_line(aes(group = Property_ID, color = FREH), alpha = 0.2)


# ------------------ How does life length vary by CMA/Geography --------------- #
# -------------------------- Boxplots and Violin Charts ----------------------- #

# life length by CMATYPE - VIOLIN CHART
boxplot_cmatype <- Canada_daily %>% 
  group_by(Property_ID) %>% 
  filter(sum(Status == "R") > 0) %>% 
  ungroup() %>% 
  filter(!is.na(CMATYPE)) %>% 
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
#ALL
medcumrev_cmatype_all <- Canada_daily %>% 
  filter(!is.na(CMATYPE))

test <-  medcumrev_cmatype_all %>% 
  group_by(CMATYPE, day_num_r) %>% 
  summarise(med_rev = mean(cumrev), n = n())

test <- test %>% 
  filter(!is.na(day_num_r))

#Early only
medcumrev_cmatype_early <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Scraped <= "2019-01-20")

test1 <-  medcumrev_cmatype_early %>% 
  group_by(CMATYPE, day_num) %>% 
  summarise(med_rev = mean(cumrev), n = n())

test1 <- test1 %>% 
  filter(!is.na(day_num))

#Late only
medcumrev_cmatype_late <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-30")

test2 <-  medcumrev_cmatype_late %>% 
  group_by(CMATYPE, day_num) %>% 
  summarise(med_rev = median(cumrev), n = n())

test2 <- test2 %>% 
  filter(!is.na(day_num))

test8 <- medcumrev_cmatype_late %>% 
  filter(CMATYPE == "Rural", day_num_r == 847 | day_num_r == 846)

#Middle only
medcumrev_cmatype_mid <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Scraped <= "2019-01-20", Created >= "2016-09-30")

test3 <-  medcumrev_cmatype_mid %>% 
  group_by(CMATYPE, day_num) %>% 
  summarise(med_rev = mean(cumrev), n = n())

test3 <- test3 %>% 
  filter(!is.na(day_num))


ggplot(data =test1, aes(x = day_num, y = med_rev))+
  geom_line(aes(group = CMATYPE, color = CMATYPE))

p <- grid.arrange(meanall, meanearly, meanlate, meanmid, ncol = 2)

prop2 <- read_csv("data/Canada_property2.csv")

early <- Canada_property %>% 
  filter(Scraped <= "2019-01-25")
late <- Canada_property %>% 
  filter(Created >= "2016-10-01")
narrow <- Canada_property %>% 
  filter(Scraped <= "2019-01-25", Created >= "2016-10-01")

################# FREH ---------------------------------------------------------------

testa <- medcumrev_cmatype_all %>% 
  group_by(FREH, day_num_r) %>% 
  summarise(med_rev = median(cumrev))
a <- ggplot(data = testa, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH))

testb <- medcumrev_cmatype_early %>% 
  group_by(FREH, day_num_r) %>% 
  summarise(med_rev = median(cumrev))
b <- ggplot(data = testb, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH))

testc <- medcumrev_cmatype_late %>% 
  group_by(FREH, day_num) %>% 
  summarise(med_rev = median(cumrev))
c<- ggplot(data = testc, aes(x = day_num, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH))

testd <- medcumrev_cmatype_mid %>% 
  group_by(FREH, day_num_r) %>% 
  summarise(med_rev = median(cumrev))
d <- ggplot(data = testd, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH))

s <- grid.arrange(a,b,c,d, ncol = 2)

################################ ml ---------------------------------------------------------------
teste <- medcumrev_cmatype_late %>% 
  group_by(ML, day_num) %>% 
  summarise(med_rev = median(cumrev))
c<- ggplot(data = testc, aes(x = day_num, y = med_rev))+
  geom_line(aes(group = FREH, color = FREH))

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
yearvyear <- Canada_daily %>% 
  filter(!is.na(day_num_r)) %>% 
  mutate(year = ifelse(Created>="2017-01-01" & Created <= "2017-12-31", "MTM",
                       ifelse(Created>="2018-01-01" & Created <= "2018-12-31", "LTM", "FTM"))) %>% 
  group_by(year, day_num_r) %>% 
  summarise(med_rev = median(cumrev))

ggplot(data = yearvyear, aes(x = day_num_r, y = med_rev))+
  geom_line(aes(group = year, color = year))
facet_wrap(~FREH)


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
                         which.max(day_num[Status == "R"]) - which.min(day_num[Status == "R"]) + 1, 0)) %>% 
  group_by(Property_ID, CMANAME) %>% 
  summarise(median(r_diff), median(lifelength))

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


# ------------------------------ Occupancy Rates by day_num -------------------------------#

#CMATYPE
occ <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  arrange(Property_ID, Date) %>% 
  group_by(Property_ID) %>% 
  mutate(r = cumsum(Status == "R")) %>%
  ungroup() %>% 
  mutate(occ_rate = r/day_num)

occ <- occ %>%   
  group_by(CMATYPE, day_num) %>% 
  summarise(occrate = mean(occ_rate))

ggplot(occ, aes(day_num, occrate))+
  geom_line(aes(group = CMATYPE, color = CMATYPE))


#CMANAME 
occ_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
  arrange(Property_ID, Date) %>% 
  group_by(Property_ID) %>% 
  mutate(r = cumsum(Status == "R")) %>%
  ungroup() %>% 
  mutate(occ_rate = r/day_num) %>% 
  group_by(CMANAME, day_num) %>% 
  summarise(occrate = mean(occ_rate))

ggplot(occ_cmaname, aes(day_num, occrate))+
  geom_line(aes(group = CMANAME, color = CMANAME), show.legend = FALSE) +
  facet_wrap(~CMANAME)


# ------------------------------ Occupancy Rates by Date -------------------------------#
#CMANAME 
occ_cmaname_date <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(CMANAME, Date) %>% 
  summarise(occrate = sum(Status == "R")/n())

ggplot(occ_cmaname_date, aes(Date, occrate))+
  geom_line(aes(group = CMANAME, color = CMANAME), show.legend = FALSE) +
  facet_wrap(~CMANAME)

#CMATYPE
occ_cmatype_date <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(CMATYPE, Date) %>% 
  summarise(occrate = sum(Status == "R")/n())

ggplot(occ_cmatype_date, aes(Date, occrate))+
  geom_line(aes(group = CMATYPE, color = CMATYPE), alpha = 0.5)+
  facet_wrap(~CMATYPE)

# ----------------------- What % of listings ever get a reservation --------------------#

perc_res <- Canada_daily %>% 
  filter(CMATYPE == "CMA" | CMATYPE == "CA") %>% 
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

created <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  mutate(month = month(Created)) %>% 
  group_by(CMATYPE, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>% 
  mutate(percent = n/sum(n))

ggplot(created, aes(month, percent))+
  geom_line(aes(color = CMATYPE))



# ----------------------- When do listings die ---------------------------------------#

dead <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(CMATYPE, month) %>% 
  summarise(n = n()) %>% 
  group_by(CMATYPE) %>% 
  mutate(percent = n/sum(n))

ggplot(dead, aes(month, percent))+
  geom_line(aes(color = CMATYPE))


# ------------------------ Average length of one reservation -------------------------#

#CMATYPE
res_length <- Canada_daily %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(CMATYPE, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(CMATYPE) %>% 
  summarise(mean(n))

ggplot(res_length, aes(CMATYPE, `mean(n)`)) +
  geom_col()

#CMANAME
res_length_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA") %>% 
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
  group_by(FREH, Reservation) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(FREH) %>% 
  summarise(mean(n))

ggplot(res_length_freh, aes(FREH, `mean(n)`)) +
  geom_col()

#ML
res_length_ml <- Canada_daily %>% 
  group_by(ML, Reservation) %>% 
  filter(sum(ML) == n() | sum(ML == 0)) %>% 
  summarise(n = n()) %>% 
  filter(n < 30) %>% 
  group_by(ML) %>% 
  summarise(mean(n))

ggplot(res_length_ml, aes(ML, `mean(n)`)) +
  geom_col()

#------------------------- Speed between ML acquisitions ----------------------#
ml <- Canada_daily %>% 
  filter(CMANAME == "Toronto", ML == TRUE) %>% 
  group_by(Property_ID) %>%
  summarise(n = n(), Created = mean(Created), Host_ID = mean(Host_ID)) %>% 
  group_by(Host_ID) %>% 
  arrange(Created, .by_group = TRUE) %>% 
  mutate(time_diff = Created - lag(Created, 1), id = row_number())

ml$Host_ID <- as.factor(ml$Host_ID)


ggplot(ml, aes(id, time_diff))+
  geom_point(aes(group = Host_ID, color = Host_ID), alpha = 0.5, show.legend = FALSE)+
  geom_smooth()


# -------------------- Differences in Price per night -----------------------#

#Date
ppn <- Canada_daily %>% 
  filter(Status == "R") %>% 
  group_by(Date) %>% 
  summarise(ppn = mean(Price))

ggplot(ppn, aes(x = day_num, y = ppn))+
  geom_line()

#Date
ppn <- Canada_daily %>% 
  filter(Status == "R", Created > "2018-10-01") %>% 
  group_by(Date) %>% 
  summarise(ppn = mean(Price))

ggplot(ppn, aes(x =Date, y = ppn))+
  geom_line()
