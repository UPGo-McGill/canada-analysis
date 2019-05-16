library(tidyverse)
library(circlize)
library(alluvial)
library(scales)
library(GGally)
library(lubridate)
library(gganimate)
library(gifski)
library(tsibble)
library(bbplot)
library(fpp)

## Rent plot
rent <- read_csv("data/CMArent.csv", locale = locale(encoding = "Latin1"))
colnames(rent) <- c("ID", "CMANAME", "med_rent")
rent[rent$CMANAME=="Lloydminster(Saskatchewan part)",]$CMANAME <- c("Lloydminster")
rent[rent$CMANAME%in%"Lloydminster(Alberta part)",]$CMANAME <- c("Lloydminster")
rent[rent$CMANAME%in%"Greater Sudbury / Grand Sudbury",]$CMANAME <- c("Greater Sudbury")
rent[rent$CMANAME%in%"Hawkesbury(Ontario part)",]$CMANAME <- c("Hawkesbury")
rent[rent$CMANAME%in%"Hawkesbury(Quebec part)" ,]$CMANAME <- c("Hawkesbury")
rent[rent$CMANAME%in%"Ottawa - Gatineau(Ontario part)",]$CMANAME <- c("Ottawa - Gatineau")
rent[rent$CMANAME%in%"Ottawa - Gatineau(Quebec part)",]$CMANAME <- c("Ottawa - Gatineau")
rent[rent$CMANAME%in%"Campbellton(New Brunswick part)",]$CMANAME <- c("Campbellton")
rent[rent$CMANAME%in%"Campbellton(Quebec part)",]$CMANAME <- c("Campbellton")
rent <- rent %>% 
  group_by(CMANAME) %>% 
  summarise(med_rent = median(med_rent))

FREH <- Canada_daily %>% 
  filter(CMATYPE =="B", (Date == "2016-01-01" | Date == "2016-02-01" | Date == "2016-03-01" | Date == "2016-04-01" | Date == "2016-05-01"
                         | Date == "2016-06-01" | Date == "2016-07-01" | Date == "2016-08-01" | Date == "2016-09-01"| Date ==  "2016-10-01"| Date ==  "2016-11-01" 
                         | Date ==  "2016-12-01" | Date == "2017-01-01" | Date == "2017-02-01" | Date == "2017-03-01" | Date == "2017-04-01" | Date == "2017-05-01"
                         | Date ==  "2017-06-01"| Date ==  "2017-07-01"| Date ==  "2017-08-01" | Date == "2017-09-01" | Date == "2017-10-01" | Date == "2017-11-01" 
                         | Date ==   "2017-12-01" | Date == "2018-01-01" | Date == "2018-02-01" | Date == "2018-03-01" | Date == "2018-04-01" | Date == "2018-05-01"),
         Housing == TRUE, Status == "R", R365 >=120, Listing_Type == "Entire home/apt") %>% 
  group_by(CMANAME, Date) %>% 
  summarise(FREHREV = median(Revenue365)/12) %>% 
  left_join(rent, by = "CMANAME") %>% 
  filter(FREHREV>0) %>% 
  mutate(ratio = FREHREV/med_rent)

  
ggplot(FREH, aes(x = CMANAME, y = ratio, fill = ratio))+
  geom_bar(stat = "identity")+
  scale_fill_gradient2(low = "#003366", mid = "#ffffff", high = "#ff0000", midpoint = 1)+
  ylim(-.5,2)+
  theme_minimal()+
  theme(aspect.ratio=1, 
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(vjust = .5),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        plot.background=element_blank())+
  coord_polar()+
  transition_time(Date)+
  labs(title = 'Ratio of FREH revenue to median rents', subtitle = 'Date: {frame_time}')

anim_save("ratio1.gif", p)

ggplot(FREH)+
  geom_point(aes(x = fct_reorder(CMANAME, med_rent), y = med_rent))+
  geom_point(aes(x = CMANAME, y = FREHREV))+
  geom_segment(aes(x = CMANAME, y = med_rent, xend = CMANAME, yend = FREHREV))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  coord_flip()

##DAILY RESERVATIONS SCATTERPLOT
daily_res2 <- Canada_daily %>% 
  filter(Status == "R") %>% 
  group_by(Date) %>% 
  summarise(n())

ggplot(daily_res2, aes(x=Date, y=`n()`)) +
  geom_point(alpha = 0.5) +
  geom_smooth() + 
  bbc_style()



## BAR CHART IDEA
daily_res <- Canada_daily %>% 
  group_by(Property_ID, Date) %>% 
  group_by(Date) %>% 
  summarise(res = n(), r_res = sum(Status == "R"), perc_r = r_res/res, perc_nr = 1 - perc_r) %>% 
  arrange(Date) %>% 
  gather(perc_nr, perc_r, key = "perc", value = "value")
  

daily_list <- Canada_daily %>% 
  mutate(month2 = month(Date)) %>%
  mutate(year = year(Date)) %>% 
  mutate(week = isoweek(Date)) %>% 
  group_by(Property_ID, week, year) %>% 
  group_by(week, year) %>% 
  summarise(n()) %>% 
  arrange(year, week) %>% 
  ungroup() %>%  
  mutate(id = row_number())

ggplot()+
  geom_bar(data = daily_res, mapping = aes(fill = perc, x = Date, y =  value), stat = "identity", position = "stack")




# PARALLEL PLOT
test <- Canada_daily %>% 
  group_by(Host_ID) %>% 
  expand.grid(.$CMANAME)


par18 <- Canada_daily %>%
  filter(Date == "2018-04-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = n(), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)

par17 <- Canada_daily %>%
  filter(Date == "2017-04-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = n(), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)

par16 <- Canada_daily %>%
  filter(Date == "2016-04-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = n(), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)


par2 <- par16[,c(1,4)] %>% 
  left_join(par17[,c(1,4)], by = "CMANAME") %>% 
  left_join(par18[,c(1,4)], by = "CMANAME") %>% 
  left_join(popCMA, by= "CMANAME")
  
alluvial(par2[,c(2:4)], freq = par2$CMA_pop, col = rainbow(35), gap.width = 0.2, cw = 0.09, blocks = FALSE, ann = FALSE)


##ALLUVIUM SMALLER TEMPORAL SCALE
par182 <- Canada_daily %>%
  filter(Date == "2018-04-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par181 <- Canada_daily %>%
  filter(Date == "2018-01-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par174 <- Canada_daily %>%
  filter(Date == "2017-10-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par173 <- Canada_daily %>%
  filter(Date == "2017-07-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par172 <- Canada_daily %>%
  filter(Date == "2017-04-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par171 <- Canada_daily %>%
  filter(Date == "2017-01-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par164 <- Canada_daily %>%
  filter(Date == "2016-10-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par163 <- Canada_daily %>%
  filter(Date == "2016-07-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par162 <- Canada_daily %>%
  filter(Date == "2016-04-30", CMATYPE == "B") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)


par2 <- par163[,c(1,4)] %>% 
  left_join(par164[,c(1,4)], by = "CMANAME") %>% 
  left_join(par171[,c(1,4)], by = "CMANAME") %>% 
  left_join(par172[,c(1,4)], by = "CMANAME") %>% 
  left_join(par173[,c(1,4)], by = "CMANAME") %>% 
  left_join(par174[,c(1,4)], by = "CMANAME") %>%
  left_join(par181[,c(1,4)], by = "CMANAME") %>% 
  left_join(par182[,c(1,4)], by = "CMANAME") %>%
  left_join(popCMA, by= "CMANAME")
colnames(par2) <- c("CMANAME", "Jul16", "Oct16", "Jan17", "Apr17", "Jul17", "Oct17", "Jan18", "Apr18", "CMA_pop")

par3 <- par163[,c(1,4)] %>% 
  left_join(par171[,c(1,4)], by = "CMANAME") %>% 
  left_join(par173[,c(1,4)], by = "CMANAME") %>% 
  left_join(par181[,c(1,4)], by = "CMANAME") %>% 
  left_join(popCMA, by= "CMANAME")
colnames(par3) <- c("CMANAME", "Jul16", "Jan17", "Jul17", "Jan18", "CMA_pop")




alluvial(par2[,c(2:9)], freq = par2$CMA_pop, col = colors, gap.width = 0, cw = 0.05, alpha = 0.75, blocks = FALSE, ann = FALSE)

colors <- c("#ea9f80", "#edaa85", "#eeab86", "#f3bd8e", "#f7d497", "#faeaa1", "#f5f1a6", "#e3e8a3",
            "#d5e2a2", "#c8dda1", "#c0d9a2", "#bcd8a2", "#bad7a1", "#bbd7a1", "#bdd9a7", "#bedbbb",
            "#bfdecd", "#bfe0e1", "#bce0ef", "#b0d6f0", "#a3b9dc", "#99a5cf", "#949ac8", "#9197c6",
            "#9397c5", "#9998c6", "#a29ac6", "#ae9cc6", "#bc9fc6", "#caa3c7", "#e5a6c5", "#e7a0b9",
            "#e99c9d", "#e89c9d", "#e99b87")






# ARC DIAGRAM
arc <- read_csv("data/arc.csv")
arc <- arc %>% 
  mutate(id = row_number())
arc <- as.data.frame(arc)
arc<- data.frame(arc[,-3], row.names = arc[,3])
circos.clear()
chordDiagram(arc)

arc2 <- arc %>%
  group_by(to) %>% 
  summarise(n())
colnames(arc2) = c("CMANAME", "num")
arc3 <- arc %>%
  group_by(from) %>% 
  summarise(n())
colnames(arc3) = c("CMANAME", "num")
arc4 <- arc2 %>% 
  full_join(arc3, by = "CMANAME")
arc4 <- arc4 %>% 
  replace(is.na(.),0) %>% 
  mutate(sum = rowSums(.[2:3]))
arc4 <- arc4 %>% 
  arrange(-sum)
arc5 <- arc %>%
  filter(to %in% c("Toronto", "Vancouver", "Montréal", "Victoria", "Ottawa - Gatineau", "Québec", "Calgary",
                   "Canmore", "Kelowna", "St. Catharines - Niagara", "Hamilton", "Edmonton", "Barrie",
                   "Kitcehener - Cambridge - Waterloo", "Sherbrooke", "London", "Nanaimo", "Collingwood",
                   "Winnipeg", "Halifax")) %>% 
  filter(from %in% c("Toronto", "Vancouver", "Montréal", "Victoria", "Ottawa - Gatineau", "Québec", "Calgary",
                     "Canmore", "Kelowna", "St. Catharines - Niagara", "Hamilton", "Edmonton", "Barrie",
                     "Kitcehener - Cambridge - Waterloo", "Sherbrooke", "London", "Nanaimo", "Collingwood",
                     "Winnipeg", "Halifax"))

arc6 <- arc5[arc5$to %in% c("Toronto", "Montréal", "Vancouver", "Victoria"),2:1]
colnames(arc6) <- colnames(arc5)
arc7 <- arc5 %>% 
  filter(to != "Toronto", to != "Montréal", to != "Vancouver", to != "Victoria")
rbind(arc7,arc6)

chordDiagram(arc5, annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function (x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1]+.1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)


## Figure 2 - donut chart
Fig2 <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Status == "R") %>% 
  group_by(CMANAME) %>%
  summarise(sum(Price), n()) %>% 
  rename(place = CMANAME)
Fig2a <- Canada_daily %>% 
  filter(!CMANAME %in% c("Montréal", "Toronto", "Vancouver"), Status == "R") %>% 
  group_by(CMATYPE) %>%
  summarise(sum(Price), n())%>% 
  rename(place = CMATYPE)
Fig2b <- rbind(Fig2, Fig2a)
Fig2b <- Fig2b %>% 
  mutate(rev = `n()`/sum(`n()`)) %>% 
  mutate(count = `sum(Price)`/sum(`sum(Price)`)) %>% 
  mutate(ymax_rev = cumsum(rev)) %>% 
  mutate(ymin_rev = replace_na(lag(ymax_rev,1),0)) %>% 
  mutate(ymax_count = cumsum(count)) %>% 
  mutate(ymin_count = replace_na(lag(ymax_count,1),0))


ggplot(data = Fig2b) + 
  geom_rect(aes(fill=rev, ymax=ymax_rev, ymin=ymin_rev, xmax=4, xmin=3), color = "white", size = 1.3, show.legend = FALSE) +
  geom_rect(aes(fill=count, ymax=ymax_count, ymin=ymin_count, xmax=3, xmin=2), color = "white", size = 1.1, show.legend = FALSE) +
  geom_text(aes(x = 3.5, y = (ymin_rev + ymax_rev)/2, label = paste(round(rev*100,0),"%")),color = "grey80") + 
  geom_text(aes(x = 2.5, y = (ymin_count + ymax_count)/2, label = paste(round(count*100,0),"%")),color = "grey80") + 
  xlim(c(0, 4)) + 
  theme(aspect.ratio=1, 
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        plot.background=element_blank())+
  coord_polar(theta = "y")



## Figure 7 - radar chart
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

Fig7 <- Canada_daily %>% 
  filter(Status == "R" & CMATYPE == "B") %>% 
  mutate(Status2 = ifelse((A365 + R365) >= 240 & R365 >= 120 & Listing_Type == "Entire home/apt", "VFREH", ifelse((A365 + R365) >= 120 & R365 >= 60 & Listing_Type == "Entire home/apt", "FREH", ifelse(Listing_Type == "Entire home/apt", "EH", "A")))) %>% 
  group_by(CMANAME, Status2) %>%
  summarise(sum(Price)) %>%
  group_by(CMANAME) %>% 
  arrange(CMANAME, desc(Status2)) %>% 
  mutate(revenue = cumsum(`sum(Price)`)) %>% 
  mutate(tot_rev = sum(`sum(Price)`)) %>%
  mutate(percent_rev = revenue/tot_rev) %>% 
  filter(Status2 != "A")

ggplot(data = Fig7, aes(x=CMANAME, y = percent_rev)) +
  geom_polygon(mapping = aes(group = Status2, color = Status2, fill = Status2)) +
  scale_fill_manual(values = c("#9DB4AB","#ABC4A1", "#BBDBD9")) +
  scale_color_manual(values = c("#9DB4AB","#ABC4A1", "#BBDBD9")) +
  coord_radar()




## Figure 6 - scatterplot
Fig6 <- Canada_daily %>% 
  filter(CMATYPE =="B" | CMATYPE =="D" | CMATYPE =="K" & Date >= "2017-01-01" & Date <= "2017-12-31") %>% 
  group_by(CMANAME) %>% 
  summarise(sum(Price[Status =="R"]), sum(Price[Status =="R" & Listing_Type == "Entire home/apt"]), mean(CMA_pop)) %>% 
  mutate(percEH = `sum(Price[Status == "R" & Listing_Type == "Entire home/apt"])`/`sum(Price[Status == "R"])`)
  
ggplot(data = Fig6) +
  geom_point(mapping = aes(x = `sum(Price[Status == "R"])`, y = percEH, size = `mean(CMA_pop)`, alpha = 0.2)) +
  scale_x_log10()




## Figure 4 - seasonality
Fig4 <- Canada_daily %>%
  filter(Date >= "2017-01-01" & Date <= "2017-12-31"& Status == "R" & CMATYPE == "B") %>% 
  mutate(month2 = month(Date)) %>%
  group_by(CMANAME, month2) %>% 
  summarise(n()) %>% 
  group_by(CMANAME) %>% 
  mutate(percent = `n()`/sum(`n()`))

ggplot(data = Fig4)+
  geom_line(aes(x = month2, y = percent, color = CMANAME))
  facet_wrap(~ CMANAME)

#Seasonal Decomposition Function 
seasonal_adjust_city <- function(CMA) {
  
ts <- Canada_daily %>% 
  filter(CMANAME == CMA) %>% 
  group_by(Date) %>% 
  summarise(num = n()) %>% 
  ts(frequency = 365, start = decimal_date(min(.$Date)))
ts <- ts[,2]
decompose_ts <- decompose(ts, "multiplicative")
decompose_ts
}

prac <- seasonal_adjust_city("Toronto")

#Old stuff seasonality
Canada_daily %>%
  filter(Date >= "2017-01-01" & Date <= "2017-12-31"& Status == "R" & CMATYPE == "B") %>% 
  mutate(month2 = month(Date)) %>%
  group_by(PRNAME, month2) %>% 
  summarise(n()) %>% 
  group_by(PRNAME) %>% 
  mutate(percent = `n()`/sum(`n()`)) %>% 
  ggplot()+
  geom_line(aes(x = month2, y = percent, color = PRNAME)) +
  facet_wrap(~ PRNAME)

df <- as_tibble(test) %>% 
  rename_all(tolower) %>% 
  mutate(date = ymd(date))

df %>% 
  mutate(
    year = factor(year(date)),     # use year to define separate curves
    date = update(date, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(date, `n()`, color = year)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_line()




##Figure 3
Fig3CMAs <- Canada_daily %>% 
  filter(CMATYPE %in% c("B", "K", "D"), Date >= "2017-01-01" & Date <= "2017-12-31") %>% 
  group_by(CMANAME, CTUID) %>% 
  summarise(sum(Price[Status =="R"]), mean(CMA_pop), mean(CT_pop)) %>% 
  group_by(CMANAME) %>%
  mutate(percrev = `sum(Price[Status == "R"])`/sum(`sum(Price[Status == "R"])`)) %>% 
  mutate(CTperc_pop = `mean(CT_pop)`/sum(`mean(CT_pop)`)) %>% 
  rename(pop = `mean(CMA_pop)`) %>% 
  arrange(CMANAME, -percrev) %>% 
  mutate(cumsum(percrev)) %>% 
  filter(`cumsum(percrev)` < 0.55) %>% 
  summarise(sum(CTperc_pop), mean(pop)) %>% 
  rename(pop2 = `mean(pop)`)
  
ggplot(data = Fig3CMAs, aes(x = fct_reorder(CMANAME, pop2), y = `sum(CTperc_pop)`)) +
  geom_point(aes(size = pop2), alpha = 0.7) +
  scale_size_continuous(range = c(1,15))+
  geom_segment(aes(x= CMANAME,
               y = `sum(CTperc_pop)`,
               xend = CMANAME,
               yend = 0), alpha = 0.5)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Fig3top10 <- Canada_daily %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Ottawa - Gatineau", "Vancouver", "Winnipeg", "Kitchener - Cambridge - Waterloo", "Québec", "Calgary", "Edmonton", "Hamilton"), Date >= "2017-01-01" & Date <= "2017-12-31") %>% 
  group_by(CMANAME, CTUID) %>% 
  summarise(sum(Price[Status =="R"]), mean(CMA_pop), mean(CT_pop)) %>% 
  group_by(CMANAME) %>%
  mutate(percrev = `sum(Price[Status == "R"])`/sum(`sum(Price[Status == "R"])`)) %>% 
  mutate(CTperc_pop = `mean(CT_pop)`/sum(`mean(CT_pop)`)) %>% 
  rename(pop = `mean(CMA_pop)`) %>% 
  arrange(CMANAME, -percrev) %>% 
  mutate(cumsum(percrev)) %>% 
  filter(`cumsum(percrev)` < 0.55) %>% 
  summarise(sum(CTperc_pop), mean(pop)) %>% 
  rename(pop2 = `mean(pop)`)
  ggplot(aes(x = CMANAME, y = `sum(CTperc_pop)`))+
  geom_bar(stat = "identity") + 
  coord_polar()

#varied width bar graph
ggplot(data = Fig3top10, aes(x = fct_reorder(CMANAME,`sum(CTperc_pop)`), y = `sum(CTperc_pop)`, width = rescale(pop2, c(.05, 1))))+
  geom_bar(stat = "identity", position = position_dodge(width = 10), fill = "turquoise") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ylim(-.1,.2)+
  coord_polar(start = 0)

  
#exploded pie chart
ggplot(data = Fig3top10, aes(x = CMANAME, y = `sum(CTperc_pop)`)) +
  geom_bar(stat = "identity", fill = "turquoise")
  

  
  
  
## Figure 5

Fig5a <- Canada_daily %>% 
  filter(CMATYPE == "CMA", Date >= "2017-01-01", Date <= "2017-12-31", Status == "R") %>% 
  group_by(CMANAME, Host_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(CMANAME) %>% 
  summarise(one = quantile(rev, c(0.99)), oneperc = sum(rev[rev>one]), `1%` = oneperc/sum(rev),
            five = quantile(rev, c(0.95)), fiveperc = sum(rev[rev>five]), `5%` = fiveperc/sum(rev), 
            ten = quantile(rev, c(0.90)), tenperc = sum(rev[rev>ten]), `10%` = tenperc/sum(rev)) %>% 
  select(CMANAME, `1%`, `5%`, `10%`) %>% 
  gather(`1%`, `5%`, `10%`, key = "percentile", value = "value") %>% 
  group_by(percentile) %>% 
  summarise(min = min(value), max = max(value), citymin = CMANAME[which.min(value)], citymax = CMANAME[which.max(value)]) %>% 
  mutate(CMATYPE = "CMA")
Fig5 <- Canada_daily %>% 
  filter(Date >= "2017-01-01" & Date <= "2017-12-31", Status == "R") %>% 
  group_by(CMATYPE, Host_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(CMATYPE) %>% 
  summarise(one = quantile(rev, c(0.99)), oneperc = sum(rev[rev>one]), `1%` = oneperc/sum(rev),
            five = quantile(rev, c(0.95)), fiveperc = sum(rev[rev>five]), `5%` = fiveperc/sum(rev), 
            ten = quantile(rev, c(0.90)), tenperc = sum(rev[rev>ten]), `10%` = tenperc/sum(rev)) %>%
  select(CMATYPE, `1%`, `5%`, `10%`) %>% 
  gather(`1%`, `5%`, `10%`, key = "percentile", value = "value") %>% 
  left_join(Fig5a)
Fig5$percentile <- factor(Fig5$percentile, levels = c('1%', '5%', '10%'))


ggplot(data = Fig5)+
  geom_bar(mapping = aes(fill = percentile, x = CMATYPE, y = value), stat = "identity", position = "dodge") +
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), lineend = "round", position = position_nudge(x = -0.3), 
               data = . %>% filter(percentile == "1%"))+
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), position = position_nudge(x = 0), 
               data = . %>% filter(percentile == "5%"))+
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), position = position_nudge(x = 0.3), 
                                                                     data = . %>% filter(percentile == "10%"))

##Figure 9
Fig9 <- read_csv("data/Fig9.csv")

ggplot(Fig9, mapping = aes(x = fct_reorder(CMANAME, Longitude), y = Perc_pop)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,1)
  


## HEAT MAP
heat <- daily %>% 
  group_by(CMANAME) %>% 
  group_by(CMANAME) %>% 
  summarise(sum(Date =="2018-04-30"), sum(Date == "2017-04-30"))
heat2 <- as.data.frame(heat)
heat2 <- heat2[1:152,]
heat2 <- data.frame(heat2[,-1], row.names = heat2[,1])
heat3 <- heat[-153,1]
heat4 <- heat[-153,]
superheat(heat4, left.label = "variable")

##MAP



myLocation <- "Montreal"
myMap <- get_map(location = myLocation, maptype = "roadmap", crop = FALSE, key = "AIzaSyCNHR8TyrpbCB6p_tn0oyxKiV0FdtE0-Aw", zoom = 12)
ggmap(myMap)+
  geom_density_2d(data = Canada_property, aes(x = Longitude, y = Latitude), size = 0)+
  stat_density2d(data = Canada_property, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.6), guide = FALSE)

ggplot() +
  geom_sf(data=MTLpoly,aes(fill = units))+
  coord_sf(xlim = c(-74.0000, -30), ylim = c(45.6000, 70))


Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C = c("01")), level = 'CT') #get CTs with population
cd <- cd %>% 
  select(GeoUID, Dwellings) %>% 
  rename(CTUID = GeoUID, units = Dwellings)

poly <- poly %>%
  left_join(cd, by = "DAUID")

ggmap::register_google(key = "AIzaSyCNHR8TyrpbCB6p_tn0oyxKiV0FdtE0-Aw")


###### GGPLOT #######
ggplot(data=MTLpoly) +
  geom_sf(color = NA, aes(fill = percFREH), size = 0.3)


bb(MTLpoly, ext = .85)




###### TMAP #########
poly <- st_read("data", "DA")
poly <- st_transform(poly, 3347)
MTLpoly <- poly %>%
  filter(CMANAME == "Montréal")
Canada_daily <- Canada_daily %>% 
  left_join(Canada_property[,c(1,20)], by = "Property_ID")
map <- Canada_daily %>%
  filter(Date == "2018-04-30") %>% 
  mutate(FREH = ifelse(sum(R365>120), 1, 0)) %>% 
  group_by(CTUID) %>% 
  summarise(FREHnum = sum(FREH))
MTLpoly <- MTLpoly %>%
  left_join(map, by = "CTUID") %>% 
  left_join(cd, by = "CTUID") %>% 
  mutate(percFREH = FREHnum/units)
breaks = c(.01, .025, .05, .1, .25)
tm_shape(MTLpoly, bbox = bb("Montreal, Quebec"))+
  tm_fill(col = "percFREH", breaks = breaks)
  

tm_facets(by = "CSDNAME")



poly <- poly %>%
  left_join(map, by = "CTUID") %>% 
  left_join(cd, by = "CTUID") %>% 
  mutate(percFREH = FREHnum/units)


CT <- st_read("data", "gct_000b11a_e")
CT <- st_transform(CT, 3347)
CT <- CT %>%
  left_join(map, by = "CTUID") %>% 
  left_join(cd, by = "CTUID") %>% 
  mutate(percFREH = FREHnum/units)
mtl <- CT %>% 
  filter(CMANAME )
tm_shape(CT, bbox = bb(CT, ext = .85))+
  tm_fill(col = "percFREH", breaks = breaks)+
  tm_facets(by = "CMANAME")



sepCMA <-
  lapply(states, function(thisState){
    ggplot(CT('state',region=thisState)) +
      geom_polygon(colour=percFREH) +
      facet_wrap(~CMANAME) +
      coord_map() +
      theme_void()
  })


library(cowplot)
plot_grid(plotlist = sepStates)










#get lat and long
Canada_property <- Canada_property %>% 
  left_join(property[,c(1,19:20)], by = "Property_ID")


daily_list <- Canada_daily %>% 
  mutate(month2 = month(Date)) %>%
  mutate(year = year(Date)) %>% 
  mutate(week = isoweek(Date)) %>% 
  group_by(Property_ID, week, year) %>% 
  group_by(week, year) %>% 
  summarise(n()) %>% 
  arrange(year, week) %>% 
  ungroup() %>%  
  mutate(id = row_number())
