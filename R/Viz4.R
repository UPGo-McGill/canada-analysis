## Visual 1-----------------------------------------------------------------------------------

occ_freh_intro <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Listing_Type == "Entire home/apt", Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(FREH8, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

threshold <- c("Booked >= 150 nights/available >= 300 nights","Booked >= 135 nights/available >= 270 nights",
  "Booked >= 120 nights/available >= 240 nights","Booked >= 105 nights/available >= 210 nights",
  "Booked >= 90 nights/available >= 183 nights","Booked >= 75 nights/available >= 150 nights",
  "Booked >= 60 nights/available >= 120 nights","Part-time")
threshold <- c("150/300","135/270",
               "120/240","105/210",
               "90/183","75/150",
               "60/120","Part-time")

eight_color <- c("#f7fcfd",
  "#e0ecf4",
  "#bfd3e6",
  "#9ebcda",
  "#8c96c6",
  "#8c6bb1",
 "#88419d",
  "#6e016b")

eight_color <- c("#4d004b","#6e016b","#88419d", "#8c6bb1", "#8c96c6", "#9ebcda","#bfd3e6", "#e0ecf4")

ggplot(data = occ_freh_intro, 
       mapping = aes(x = day_num_r,
                     y = occrate,
                     color = FREH8))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, alpha = .8, span = 0.2)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  annotate("text", x = 450, y = .73, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = eight_color, name = "Minimum nights \nbooked and available", labels = threshold)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))

## Visual 2 (creation overall)----------------------------------------------------------------
#overall creation function
seasonal_adjust_creation<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    filter(Created >= "2017-01-01", 
           Created <= "2018-12-31") %>% 
    mutate(month = as.yearmon(Created)) %>% 
    group_by(Property_ID, month) %>% 
    summarise(n = n()) %>% 
    group_by(month) %>% 
    summarise(n = n())
  ts <- ts %>% 
    ts(frequency = 12)
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}

#Overall revenue function
seasonal_adjust_rev<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    mutate(month = as.yearmon(Date)) %>% 
    group_by(month) %>% 
    summarise(rev = sum(Price[Status == "R"]))
  ts <- ts %>% 
    filter(month<= "Dec 2018" & 
             month>= "Jan 2017") %>% 
    ts(frequency = 12)
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}

#run functions
allcreate <- seasonal_adjust_creation(fulltime = c("FREH", "VFREH", "Part-time"))
allrev <- seasonal_adjust_rev(fulltime = c("FREH", "VFREH", "Part-time"))

#create visual
allcreate <- allcreate$seasonal[1:12]/12
month <- c("Jan", "Feb", "Mar", "Apr", 
           "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
allcreate2 <- as.data.frame(month, allcreate)
allcreate <- allcreate2 %>% 
  mutate(percent = allcreate) %>%
  mutate(Region = "Creations")
rm(allcreate2)

allrev <- allrev$seasonal[1:12]/12
allrev2 <- as.data.frame(month, allrev)
allrev <- allrev2 %>% 
  mutate(percent = allrev) %>%
  mutate(Region = "Revenue")
rm(allrev2)

allcreate <- rbind(allcreate, allrev)
allcreate$month <- factor(allcreate$month, levels = c("Jan", "Feb", "Mar", 
                                                      "Apr", "May", "Jun", 
                                                      "Jul", "Aug", "Sep", 
                                                      "Oct", "Nov", "Dec"))

two_color <- c("#8c6bb1", "#9ebcda")
ggplot()+
  geom_line(data = allcreate,
            mapping = aes(x = month, 
                          y = percent, 
                          color = Region, 
                          group = Region), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  xlab("Month")+
  ylab("Percentage of activity")+
  scale_color_manual(name = "", values = two_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## Visual 3 (creation by freh) ----------------------------------------------------------------
created_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), 
         Housing == TRUE, 
         Created >= "2017-01-01", 
         Created <= "2017-12-31", 
         Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2) %>% 
  mutate(percent = n/sum(n))
month3 <- c("Jan", "Feb", "Mar", 
            "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", 
            "Oct", "Nov", "Dec")
created_freh <- created_freh %>% 
  mutate(month2 = month3)
created_freh$month2 <- factor(created_freh$month2, 
                              levels = c("Jan", "Feb", "Mar", 
                                         "Apr", "May", "Jun", 
                                         "Jul", "Aug", "Sep", 
                                         "Oct", "Nov", "Dec"))
created_freh$FREH2 <- coalesce(created_freh$FREH2, c("Part-time"))
created_freh$FREH2 <- factor(created_freh$FREH2, levels = c("Part-time", "FREH", "VFREH"))
three_color <- c("#8c6bb1", "#9ebcda", "#0c316b")

ggplot(data = created_freh)+
  geom_line(mapping = aes(month2, percent, group = FREH2, color = FREH2), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_y_continuous(labels = scales::percent_format())+
  scale_colour_manual(values = three_color, name = "")+
  xlab("Month")+
  ylab("Percentage of creations")


## Visual 4 (creation by CMAgroup) ----------------------------------------------------------------
seasonal_adjust_creation_c<- function(cmagroup) {
  ts <- Canada_daily_seas %>%
    filter(Created >= "2017-01-01", 
           Created <= "2018-12-31",
           CMAgroup == cmagroup) %>% 
    mutate(month = as.yearmon(Created)) %>% 
    group_by(Property_ID, month) %>% 
    summarise(n = n()) %>% 
    group_by(month) %>% 
    summarise(n = n())
  ts <- ts %>% 
    ts(frequency = 12)
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}

bigts <- seasonal_adjust_creation_c(cmagroup = "big")
smallts <- seasonal_adjust_creation_c(cmagroup = "small")
midts <- seasonal_adjust_creation_c(cmagroup = "mid")
touristts <- seasonal_adjust_creation_c(cmagroup = "tourist")

bigts <- bigts$seasonal[1:12]/12
month <- c("Jan", "Feb", "Mar", "Apr", 
           "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
bigts2 <- as.data.frame(month, bigts)
bigts <- bigts2 %>% 
  mutate(percent = bigts) %>%
  mutate(Region = "Big CMAs")
rm(bigts2)

smallts <- smallts$seasonal[1:12]/12
smallts2 <- as.data.frame(month, smallts)
smallts <- smallts2 %>% 
  mutate(percent = smallts) %>%
  mutate(Region = "Small CMAs")
rm(smallts2)

midts <- midts$seasonal[1:12]/12
month <- c("Jan", "Feb", "Mar", "Apr", 
           "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
midts2 <- as.data.frame(month, midts)
midts <- midts2 %>% 
  mutate(percent = midts) %>%
  mutate(Region = "Mid-Sized CMAs")
rm(midts2)

touristts <- touristts$seasonal[1:12]/12
touristts2 <- as.data.frame(month, touristts)
touristts <- touristts2 %>% 
  mutate(percent = touristts) %>%
  mutate(Region = "Touristy CMAs")
rm(touristts2)

touristtscreate <- rbind(touristts, bigts, smallts, midts)
touristtscreate$month <- factor(allcreate$month, levels = c("Jan", "Feb", "Mar", 
                                                            "Apr", "May", "Jun", 
                                                            "Jul", "Aug", "Sep", 
                                                            "Oct", "Nov", "Dec"))

four_color <- c("#2d012d","#8c6bb1", "#9ebcda", "#0c316b")
ggplot()+
  geom_line(data = touristtscreate,
            mapping = aes(x = month, 
                          y = percent, 
                          color = Region, 
                          group = Region), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  xlab("Month")+
  ylab("Percentage of creations")+
  scale_color_manual(name = "", values = four_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## Viz 5 (time between ML acquisition) --------------------------------------------------------------
ml <- Canada_daily %>% 
  filter(MLnew == TRUE, Housing == TRUE, !is.na(CMATYPE)) %>% 
  group_by(Property_ID) %>%
  summarise(n = n(), Created = mean(Created), Host_ID = mean(Host_ID)) %>% 
  group_by(Host_ID) %>% 
  arrange(Created, .by_group = TRUE) %>% 
  mutate(time_diff = Created - lag(Created, 1), id = row_number())

ml$Host_ID <- as.factor(ml$Host_ID)

ml2 <- ml %>% 
  group_by(id) %>% 
  summarise(g = mean(time_diff))

ml2 %>% 
  filter(id<=200) %>% 
  ggplot(aes(id, g))+
  geom_col(color = "#0c316b", fill = "#0c316b", show.legend = FALSE)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  xlab("Number of listings managed")+
  ylab("Number of days between acquisitions")

## Viz 6 (overall ramp up period duration) --------------------------------------------------------------

ramp_up <- Canada_daily %>% 
  filter(Housing == TRUE, 
         !is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Created <= "2018-08-01", 
         Scraped <= "2019-01-25", 
         day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month) %>% 
  summarise(mean(x, na.rm = TRUE))

ramp_up2 <- Canada_daily %>% 
  filter(Housing == TRUE, 
         !is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Created <= "2018-08-01", 
         Scraped <= "2019-01-25", 
         day_num_r == 1) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180)

mean(ramp_up2$x)
median(ramp_up2$x)

ggplot(data = ramp_up2, mapping = aes(x))+
  geom_histogram(binwidth = 1, fill = "#0c316b")+
  geom_segment(aes(x = mean(ramp_up2$x), y = 0, xend = mean(ramp_up2$x), yend = 3000), 
               lwd = .5, color = "grey50", linetype = 3)+
  geom_segment(aes(x = median(ramp_up2$x), y = 0, xend = median(ramp_up2$x), yend = 3000), 
               lwd = .5, color = "grey50", linetype = 3)+
  annotate("text", x = 10, y = 2900, label = "Median", color = "grey50")+
  annotate("text", x = 40, y = 2200, label = "Mean", color = "grey50")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_y_continuous(labels = scales::comma_format())+
  xlab("Duration of ramp-up period in days")+
  ylab("Number of listings")

ggplot(data = ramp_up, mapping = aes(x = month, y = `mean(x, na.rm = TRUE)`))+
  geom_col(fill = "#0c316b")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(size = .09, color = "grey10"))+
  ylab("Duration of ramp-up period\n(days)")+
  xlab("Creation month")

## Viz 7 (FREH ramp up period duration) --------------------------------------------------------------

ramp_up_freh <- Canada_daily %>% 
  filter(Housing == TRUE, 
         !is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Created <= "2018-08-01", 
         Scraped <= "2019-01-25", 
         Listing_Type == 'Entire home/apt', 
         day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, FREH2) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, FREH2) %>% 
  summarise(mean(x, na.rm = TRUE))
ramp_up_freh$FREH2 <- coalesce(ramp_up_freh$FREH2, c("Part-time"))
ramp_up_freh$FREH2 <- factor(ramp_up_freh$FREH2, levels = c("Part-time", "FREH", "VFREH"))
three_color <- c("#8c6bb1", "#9ebcda", "#0c316b")

ggplot(data = ramp_up_freh, mapping = aes(x = month, y = `mean(x, na.rm = TRUE)`))+
  geom_line(aes(group = FREH2, color = FREH2), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  ylab("Duration of ramp-up period\n(days)")+
  xlab("Creation month")

## Viz 8 (CMA type ramp up period duration) --------------------------------------------------------------
ramp_up_cmatype <- Canada_daily %>% 
  filter(Housing == TRUE, 
         !is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Scraped <= "2019-01-25", 
         day_num_r == 1) %>% 
  mutate(month = format(as.Date(Created), "%Y-%m")) %>% 
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID, month, CMATYPE) %>%
  summarise(x = mean(diff, na.rm = TRUE)) %>% 
  filter(x <= 180) %>% 
  group_by(month, CMATYPE) %>% 
  summarise(mean(x, na.rm = TRUE))

ramp_up_cmatype %>% 
  filter(month <= "2018-08") %>% 
ggplot(mapping = aes(x = month, y = `mean(x, na.rm = TRUE)`))+
  geom_line(aes(group = CMATYPE, color = CMATYPE), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  ylab("Duration of ramp-up period\n(days)")+
  xlab("Creation month")



## Viz 9 (Occupancy Rate and PPN Overall) --------------------------------------------------------------



## Viz 9.5 (Occupancy Rate and PPN by month) --------------------------------------------
ppnmed <- Canada_daily %>% 
  filter(Status == "R",
         Created >= "2016-09-01",
         Housing == TRUE, 
         !(is.na(CMATYPE))) %>% 
  group_by(day_num_r) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

occ <- Canada_daily %>% 
  filter(!is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Housing == TRUE) %>% 
  group_by(day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())


one_color <- c("#8c6bb1")
ggplot(data = ppnmed, mapping = aes(x = day_num_r, y = ppnCAD))+
  geom_line(alpha = 0.2, color = "#8c6bb1")+
  geom_smooth(se = FALSE, span = 0.3, alpha = 0.8, color = "#8c6bb1")+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 150), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y =40, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = one_color, name = "")+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))

ggplot(data = occ, mapping = aes(x = day_num_r, y = occrate))+
  geom_line(alpha = 0.2, color = "#8c6bb1")+
  geom_smooth(se = FALSE, span = 0.3, alpha = 0.8, color = "#8c6bb1")+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y = .55, label = "1-year mark", color = "grey30")+
  #geom_smooth(se = FALSE, lwd = 0.6)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = one_color, name = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))



## Viz 10 (Occupancy Rate and PPN Overall by month of creation) --------------------------------------------------------------
ppnmed_mon <- Canada_daily %>% 
  filter(Status == "R",
         Created >= "2016-09-01",
         Housing == TRUE, 
         !(is.na(CMATYPE))) %>% 
  mutate(month = format(as.Date(Created), "%m")) %>%
  group_by(day_num_r, month) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

occ_mon <- Canada_daily %>% 
  filter(!is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Housing == TRUE) %>% 
  mutate(month = format(as.Date(Created), "%m")) %>%
  group_by(month, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_mon2 <- occ_mon %>% 
  filter(n>49)
month3 <- c("Jan", "Feb", "Mar", 
            "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", 
            "Oct", "Nov", "Dec")

twelve_color <- c("#023858","#045a8d","#0570b0","#3690c0","#74a9cf","#a6bddb",
"#9ebcda",
"#8c96c6",
"#8c6bb1",
"#88419d",
"#810f7c",
"#4d004b")

ggplot(data = ppnmed_mon, mapping = aes(x = day_num_r, y = ppnCAD, color = month))+
  geom_smooth(alpha = 0.8, se = FALSE)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 150), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y =40, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = twelve_color, name = "", labels = month3)+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))
  
ggplot(data = occ_mon2, mapping = aes(x = day_num_r, y = occrate, color = month))+
    geom_line(alpha = 0.8)+
    geom_segment(aes(x = 365,
                     xend = 365,
                     y = 0,
                     yend = 1), color = "grey50", linetype = 3)+
    annotate("text", x = 430, y = .55, label = "1-year mark", color = "grey30")+
    #geom_smooth(se = FALSE, lwd = 0.6)+
    theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
          panel.grid.minor.x = element_blank(),
          text=element_text(size=10),
          axis.text = element_text(size = 10),
          panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
          panel.grid.minor.y = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.justification = "center",
          panel.background=element_blank(),
          axis.line = element_line(size = .09, color = "grey10"))+
    scale_color_manual(values = twelve_color, name = "", labels = month3)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    ylab("Occupancy rate")+
    xlab("Days since first reservation")+
    scale_x_continuous(limits = c(0,730))

## Viz 11 (Occupancy Rate and PPN Overall by cmatype) --------------------------------------------------------------

ppnmed_cmatype <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, CMATYPE) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)


three_color <- c("#8c6bb1", "#9ebcda", "#0c316b")

ggplot(data = ppnmed_cmatype, mapping = aes(x = day_num_r, y = ppnCAD, fill = CMATYPE, color = CMATYPE))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 150), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y =40, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  scale_fill_manual(values = three_color, name = "")+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))

occ_cmatype <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMATYPE, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

ggplot(data = occ_cmatype, mapping = aes(x = day_num_r, y = occrate, fill = CMATYPE, color = CMATYPE))+
  geom_line(alpha = 0.8)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  geom_smooth(se = FALSE, span = 0.3, alpha = 0.8)+
  annotate("text", x = 430, y = .55, label = "1-year mark", color = "grey30")+
  #geom_smooth(se = FALSE, lwd = 0.6)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  scale_fill_manual(values = three_color, name = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))

## Viz 12 (Occupancy Rate and PPN Overall by cmagroup) --------------------------------------------------------------
ppnmed_cmaname <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, CMATYPE == "CMA") %>% 
  group_by(day_num_r, CMAgroup) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

occ_cmaname <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(CMAgroup, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

ggplot(data = ppnmed_cmaname, 
       mapping = aes(x = day_num_r, 
                     y = ppnCAD, 
                     color = CMAgroup))+
  geom_line(alpha = 0.2)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 150), color = "grey50", linetype = 3)+
  geom_smooth(se = FALSE, span = 0.3, alpha = 0.8)+
  annotate("text", x = 430, y =145, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = four_color, name = "", labels = c("Large cities", "Mid-sized cities",
                                                                "Small cities", "Touristy cities"))+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))


ggplot(data = occ_cmaname, mapping = aes(x = day_num_r, 
                                         y = occrate, 
                                         color = CMAgroup))+
  geom_line(alpha = 0.2)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  annotate("text", x = 430, y = .55, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = four_color, name = "",labels = c("Large cities", "Mid-sized cities",
                                                               "Small cities", "Touristy cities"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))

## Viz 13 (Occupancy Rate and PPN Overall by LT) --------------------------------------------------------------

occ_lt <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Listing_Type, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())
ppnmed_lt <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(day_num_r, Listing_Type) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)

ggplot(data = ppnmed_lt, 
       mapping = aes(x = day_num_r, 
                     y = ppnCAD, 
                     color = Listing_Type))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 150), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y =100, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))


ggplot(data = occ_lt, mapping = aes(x = day_num_r, 
                                         y = occrate, 
                                         color = Listing_Type))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y = .55, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))

## Viz 14 (Occupancy Rate and PPN Overall by freh) --------------------------------------------------------------
occ_freh <- Canada_daily %>% 
  filter(!is.na(CMATYPE), 
         Listing_Type == "Entire home/apt", 
         Created >= "2016-09-01", 
         Housing == TRUE) %>% 
  group_by(FREH2, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())
occ_freh$FREH2 <- coalesce(occ_freh$FREH2, c("Part-time"))
occ_freh$FREH2 <- factor(occ_freh$FREH2, levels = c("Part-time", "FREH", "VFREH"))

ppnmed_freh <- Canada_daily %>% 
  filter(Status == "R", 
         !(is.na(CMATYPE)),
         Created >= "2016-09-01", 
         Listing_Type == "Entire home/apt", 
         Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(FREH2, day_num_r) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)
ppnmed_freh$FREH2 <- coalesce(ppnmed_freh$FREH2, c("Part-time"))
ppnmed_freh$FREH2 <- factor(ppnmed_freh$FREH2, levels = c("Part-time", "FREH", "VFREH"))

ggplot(data = ppnmed_freh, 
       mapping = aes(x = day_num_r, 
                     y = ppnCAD, 
                     color = FREH2))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 270), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y =80, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))


ggplot(data = occ_freh, mapping = aes(x = day_num_r, 
                                    y = occrate, 
                                    color = FREH2))+
  geom_line(alpha = 0.2)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  annotate("text", x = 430, y = .73, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = three_color, name = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))

## Viz 15 (Occupancy Rate and PPN Overall by ML) --------------------------------------------------------------

ppnmed_ml <- Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE))) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(MLnew)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  group_by(ML2, day_num_r) %>% 
  summarise(ppn = median(Price), n = n(), ppnCAD = ppn*1.2957)
ppnmed_ml$ML2 <- factor(ppnmed_ml$ML2, levels = c("TRUE", "FALSE"))


occ_ml <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(Property_ID) %>% 
  mutate(ML2 = ifelse(sum(MLnew)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  group_by(ML2, day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())
occ_ml$ML2 <- factor(occ_ml$ML2, levels = c("TRUE", "FALSE"))

ggplot(data = ppnmed_ml, 
       mapping = aes(x = day_num_r, 
                     y = ppnCAD, 
                     color = ML2))+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  geom_line(alpha = 0.2)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 200), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y =160, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = two_color, name = "", labels = c("Multilisting", "Non-Multilisting"))+
  ylab("Median nightly price (CAD)")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))


ggplot(data = occ_ml, mapping = aes(x = day_num_r, 
                                      y = occrate, 
                                      color = ML2))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, span = 0.2, alpha = 0.8)+
  geom_segment(aes(x = 365,
                   xend = 365,
                   y = 0,
                   yend = 1), color = "grey50", linetype = 3)+
  annotate("text", x = 430, y = .73, label = "1-year mark", color = "grey30")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_color_manual(values = two_color, name = "", labels = c("Multilisting", "Non-Multilisting"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Occupancy rate")+
  xlab("Days since first reservation")+
  scale_x_continuous(limits = c(0,730))

## Viz 16 (Wind-down period duration by month) --------------------------------------------------------------

winddown_mon <- Canada_daily %>% 
  filter(Housing == TRUE, 
         !is.na(CMATYPE), 
         Created >= "2016-09-01", 
         Scraped <= "2018-01-25") %>%
  mutate(month = format(as.Date(Created), "%m")) %>% 
  group_by(Property_ID, month) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  group_by(month) %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

ggplot(data = winddown_mon, mapping = aes(x = month, y = deathdiff))+
  geom_col(fill = "#0c316b")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(size = .09, color = "grey10"))+
  ylab("Duration of wind-down period")+
  xlab("Creation month")


## Viz 17 (Overall creations vs revenue vs death) ------------------------------------------------------------

#overall creation function
seasonal_adjust_creation<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    filter(Created >= "2017-01-01", 
           Created <= "2018-12-31") %>% 
    mutate(month = as.yearmon(Created)) %>% 
    group_by(Property_ID, month) %>% 
    summarise(n = n()) %>% 
    group_by(month) %>% 
    summarise(n = n())
  ts <- ts %>% 
    ts(frequency = 12)
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}

#overall takedown function
seasonal_adjust_dead<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    filter(Created >= "2016-09-01", 
           Created <= "2018-12-31", 
           Scraped <= "2018-12-31") %>% 
    mutate(month = as.yearmon(Scraped)) %>% 
    group_by(Property_ID, month) %>% 
    summarise(n = n()) %>% 
    group_by(month) %>% 
    summarise(n = n())
  ts <- ts %>% 
    ts(frequency = 12)
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}

#Overall revenue function
seasonal_adjust_rev<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    mutate(month = as.yearmon(Date)) %>% 
    group_by(month) %>% 
    summarise(rev = sum(Price[Status == "R"]))
  ts <- ts %>% 
    filter(month<= "Dec 2018" & 
             month>= "Jan 2017") %>% 
    ts(frequency = 12)
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}

#run functions
allcreate <- seasonal_adjust_creation(fulltime = c("FREH", "VFREH", "Part-time"))
allrev <- seasonal_adjust_rev(fulltime = c("FREH", "VFREH", "Part-time"))
alldead <- seasonal_adjust_dead(fulltime = c("FREH", "VFREH", "Part-time"))

#create visual
allcreate <- allcreate$seasonal[1:12]/12
month <- c("Jan", "Feb", "Mar", "Apr", 
           "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
allcreate2 <- as.data.frame(month, allcreate)
allcreate <- allcreate2 %>% 
  mutate(percent = allcreate) %>%
  mutate(Region = "Creations")
rm(allcreate2)

allrev <- allrev$seasonal[1:12]/12
allrev2 <- as.data.frame(month, allrev)
allrev <- allrev2 %>% 
  mutate(percent = allrev) %>%
  mutate(Region = "Revenue")
rm(allrev2)

alldead <- alldead$seasonal[1:12]/12
alldead2 <- as.data.frame(month, alldead)
alldead <- alldead2 %>% 
  mutate(percent = alldead) %>%
  mutate(Region = "Takedowns")
rm(alldead2)


allcreate <- rbind(allcreate, allrev)
allcreate$month <- factor(allcreate$month, levels = c("Jan", "Feb", "Mar", 
                                                      "Apr", "May", "Jun", 
                                                      "Jul", "Aug", "Sep", 
                                                      "Oct", "Nov", "Dec"))

ggplot()+
  geom_line(data = allcreate,
            mapping = aes(x = month, 
                          y = percent, 
                          color = Region, 
                          group = Region),
            lwd = 1.8,
            alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  xlab("Month")+
  ylab("Percentage of activity")+
  scale_color_manual(name = "", values = three_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## Viz 18 (Takedowns by freh) ------------------------------------------------------------

dead_freh <- Canada_daily %>% 
  filter(!(is.na(CMATYPE)), 
         Scraped <= "2018-12-31", 
         Housing == TRUE, 
         Scraped >= "2018-01-01", 
         Listing_Type == "Entire home/apt") %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(FREH2, month) %>% 
  summarise(n = n()) %>% 
  group_by(FREH2) %>% 
  mutate(percent = n/sum(n))
month3 <- c("Jan", "Feb", "Mar", 
            "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", 
            "Oct", "Nov", "Dec")
dead_freh <- dead_freh %>% 
  mutate(month2 = month3)
dead_freh$month2 <- factor(dead_freh$month2, 
                              levels = c("Jan", "Feb", "Mar", 
                                         "Apr", "May", "Jun", 
                                         "Jul", "Aug", "Sep", 
                                         "Oct", "Nov", "Dec"))
dead_freh$FREH2 <- coalesce(dead_freh$FREH2, c("Part-time"))
dead_freh$FREH2 <- factor(dead_freh$FREH2, levels = c("Part-time", "FREH", "VFREH"))
three_color <- c("#8c6bb1", "#9ebcda", "#0c316b")

ggplot(data = dead_freh)+
  geom_line(mapping = aes(month2, percent, group = FREH2, color = FREH2), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_colour_manual(values = three_color, name = "")+
  xlab("Month")+
  ylab("Percentage of takedowns")

## Viz 19 (Increasing price per night) ----------------------------------------------------------
price2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01", Status == "R") %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>% 
  group_by(year, day_num_r) %>% 
  summarise(ppn = median(Price)) %>% 
  mutate(ppnCAD = ifelse(year == "LTM", ppn*1.2957,
                         ifelse(year == "MTM", ppn*1.2986, "NA")))

price4 <- price2 %>% 
  filter(day_num_r<=365, !(is.na(year)))

ggplot(data = price4, aes(x = day_num_r, y = ppnCAD, color = year))+
  geom_line(alpha = 0.2)+
  geom_smooth(se = FALSE, alpha = 0.8, span = .2)+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))+
  scale_colour_manual(values = two_color, name = "", labels = c("2018", "2017"))+
  xlab("Days since first reservation")+
  ylab("Price (CAD)")

## Viz 20 (Length of life by death month) --------------------------------------------------------
dd <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Scraped <= "2019-01-25") %>% 
  mutate(length = Scraped - Created,
         scraped = format(as.Date(Scraped), "%Y-%m")) %>%
  group_by(Property_ID, scraped, length) %>% 
  summarise(n()) %>% 
  group_by(scraped) %>% 
  summarise(median(length))

ggplot(dd, aes(x = scraped, y = `median(length)`))+
  geom_col(fill = "#0c316b")+
  theme(panel.grid.major.x = element_line(size = 0.03, color = "grey90"),
              panel.grid.minor.x = element_blank(),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
              panel.grid.major.y = element_line(size = 0.03, color = "grey90"),
              panel.grid.minor.y = element_blank(),
              legend.key = element_blank(),
              legend.position = "bottom",
              legend.justification = "center",
              panel.background=element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line = element_line(size = .09, color = "grey10"))+
          ylab("Duration of existence")+
          xlab("Takedown date")
