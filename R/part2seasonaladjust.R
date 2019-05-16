## Overall creations vs revenue

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
                          group = Region))+
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
  xlab("Month")+
  ylab("Percent of activity")+
  scale_color_manual(name = "", values = two_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


#creations by cmagroup
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
  mutate(Region = "Tourist CMAs")
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
                          group = Region))+
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
  xlab("Month")+
  ylab("Percent of activity")+
  scale_color_manual(name = "", values = four_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


#creations by fulltime status ------------------------------------------------------------------------
seasonal_adjust_creation_f<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    filter(Created >= "2017-01-01", 
           Created <= "2018-12-31",
           FREH2 == fulltime) %>% 
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

na_seasonal_adjust_creation_f<- function(fulltime) {
  ts <- Canada_daily_seas %>%
    filter(Created >= "2016-09-01", 
           Created <= "2018-12-31",
           is.na(FREH2)) %>% 
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

frehts <- seasonal_adjust_creation_f(fulltime = "FREH")
vfrehts <- seasonal_adjust_creation_f(fulltime = "VFREH")
parttimets <- na_seasonal_adjust_creation_f(fulltime = NA)

frehts <- frehts$seasonal[1:12]/12
month <- c("Jan", "Feb", "Mar", "Apr", 
           "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
frehts2 <- as.data.frame(month, frehts)
frehts <- frehts2 %>% 
  mutate(percent = frehts) %>%
  mutate(Region = "FREH")
rm(frehts2)

vfrehts <- vfrehts$seasonal[1:12]/12
vfrehts2 <- as.data.frame(month, vfrehts)
vfrehts <- vfrehts2 %>% 
  mutate(percent = vfrehts) %>%
  mutate(Region = "VFREH")
rm(vfrehts2)

parttimets <- parttimets$seasonal[1:12]/12
parttimets2 <- as.data.frame(month, parttimets)
parttimets <- parttimets2 %>% 
  mutate(percent = parttimets) %>%
  mutate(Region = "Part-Time")
rm(parttimets2)

frehts <- rbind(frehts, parttimets)
frehts$month <- factor(frehts$month, levels = c("Jan", "Feb", "Mar", 
                                                            "Apr", "May", "Jun", 
                                                            "Jul", "Aug", "Sep", 
                                                            "Oct", "Nov", "Dec"))

three_color <- c("#8c6bb1", "#9ebcda", "#0c316b")
ggplot()+
  geom_line(data = frehts,
            mapping = aes(x = month, 
                          y = percent, 
                          color = Region, 
                          group = Region))+
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
  xlab("Month")+
  ylab("Percent of activity")+
  scale_color_manual(name = "", values = two_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## Overall creations vs revenue Vs death ------------------------------------------------------------

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


allcreate <- rbind(allcreate, allrev, alldead)
allcreate$month <- factor(allcreate$month, levels = c("Jan", "Feb", "Mar", 
                                                      "Apr", "May", "Jun", 
                                                      "Jul", "Aug", "Sep", 
                                                      "Oct", "Nov", "Dec"))

ggplot()+
  geom_line(data = allcreate,
            mapping = aes(x = month, 
                          y = percent, 
                          color = Region, 
                          group = Region))+
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
  xlab("Month")+
  ylab("Percent of activity")+
  scale_color_manual(name = "", values = three_color)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
