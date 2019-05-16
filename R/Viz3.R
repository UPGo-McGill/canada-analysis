####### libraries #######

library(tidyverse)
library(circlize)
#library(alluvial)
library(scales)
#library(GGally)
library(lubridate)
#library(gganimate)
#library(gifski)
library(tsibble)
#library(bbplot)
library(fpp)
library(grid)
library(ggforce)

####### recreated visuals #########

### figure 2 donut chart ###

Fig2a <- Candaa_daily_red4 %>% 
  ungroup() %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(CMANAME, Date) %>%
  summarise(rev = sum(Price[Status == "R"]), n = n()) %>% 
  group_by(CMANAME) %>% 
  summarise(rev = sum(rev), n = mean(n)) %>% 
  rename(place = CMANAME)
Fig2aa <- Candaa_daily_red4 %>% 
  ungroup() %>% 
  filter(!CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(CMATYPE, Date) %>%
  summarise(rev = sum(Price[Status == "R"]), n = n())%>% 
  group_by(CMATYPE) %>% 
  summarise(rev = sum(rev), n = mean(n)) %>% 
  rename(place = CMATYPE)
Fig2ba <- bind_rows(Fig2a, Fig2aa)
Fig2ba <- Fig2ba %>% 
  slice(c(2,1,3,5,4,6)) %>% 
  mutate(count = n/sum(n)) %>% 
  mutate(rev = rev/sum(rev)) %>% 
  mutate(ymax_rev = cumsum(rev)) %>% 
  mutate(ymin_rev = replace_na(lag(ymax_rev,1),0)) %>% 
  mutate(ymax_count = cumsum(count)) %>% 
  mutate(ymin_count = replace_na(lag(ymax_count,1),0))

Fig2ba$place <- factor(Fig2ba$place, levels = c("Toronto", "Montréal", "Vancouver", "CMA", "CA", "Rural"))

ggplot(data = Fig2ba) + 
  geom_rect(aes(fill=place, ymax=ymax_rev, ymin=ymin_rev, xmax=3, xmin=2), color = "white", size = 1.3, alpha = 0.8) +
  geom_rect(aes(fill=place, ymax=ymax_count, ymin=ymin_count, xmax=4, xmin=3), color = "white", size = 1.1, alpha = 0.8) +
  geom_text(aes(x = 2.5, y = (ymin_rev + ymax_rev)/2, label = paste(round(rev*100,0),"%")),color = "white", size = 3.4) + 
  geom_text(aes(x = 3.5, y = (ymin_count + ymax_count)/2, label = paste(round(count*100,0),"%")),color = "white", size = 3.4) + 
  geom_text(aes(x = 1.1, y = .83, label = paste("Revenue")) ,color = "grey40", size = 3.5) + 
  geom_text(aes(x = 5.3, y = .8, label = paste("Active Listings")) ,color = "grey40", size = 3.5) + 
  xlim(c(0, 5.4)) + 
  theme(aspect.ratio=1, 
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(color = "grey40", size =10),
        legend.position=c(.9, 0.5),
        panel.background=element_blank(),
        panel.border=element_blank(),
        plot.background=element_blank())+
  coord_polar(theta = "y")+
  scale_fill_manual(name = "", values = Fig2col, breaks = c("Toronto", "Montréal", "Vancouver", "CMA", "CA", "Rural"))

Fig2col <- c("#2d012d","#59157c" , "#8c6bb1", "#9ebcda","#6c8bb7", "#0c316b")


## figure 3 lollipop chart ##

Fig3a <- Candaa_daily_red4 %>% 
  filter(CMATYPE %in% c("CMA")) %>% 
  group_by(CMANAME, CTUID, PRNAME) %>% 
  summarise(rev = sum(Price[Status =="R"]), CT_pop = mean(CT_pop), rev_dens = rev/CT_pop, CMA_pop = mean(CMA_pop)) %>% 
  group_by(CMANAME, PRNAME) %>%
  mutate(percrev = rev/sum(rev)) %>% 
  mutate(CTperc_pop = CT_pop/sum(CT_pop, na.rm = TRUE)) %>% 
  arrange(CMANAME, -rev_dens) %>% 
  mutate(new = cumsum(percrev), cumCTperc_pop = cumsum(CTperc_pop)) %>% 
  mutate(totalrev = sum(rev)) %>% 
  filter(cumCTperc_pop >= 0.1) %>% 
  summarize(cum_rev = first(new), CANrev = mean(totalrev)/1405932782, CMA_pop = mean(CMA_pop)) %>% 
  mutate(Region = ifelse(PRNAME %in% c("British Columbia / Colombie-Britannique"), "British Columbia",
                         ifelse(PRNAME %in% c("Ontario"), "Ontario", 
                                ifelse(CMANAME %in% c("Alma", "Baie-Comeau", "Cowansville", "Dolbeau-Mistassini", "Drummondville",
                                                      "Granby", "Joliette", "Lachute", "Matane", "Montréal", "Québec", "Rouyn-Noranda", "Rimouski",
                                                      "Rivière-du-Loup", "Saguenay", "Saint-Georges", "Saint-Hyacinthe", "Sainte-Marie", "Salaberry-de-Valleyfield",
                                                      "Sept-Îles", "Shawinigan", "Sorel-Tracey", "Sherbrooke", "Thetford Mines", "Trois-Rivières",
                                                      "Val d'Or", "Victoriaville"), "Quebec",
                                       ifelse(PRNAME %in% c("Alberta", "Saskatchewan", "Manitoba"), "Prairies","Atlantic Canada"))))) %>% 
  mutate(Region2 = ifelse(PRNAME %in% c("British Columbia / Colombie-Britannique"), "British Columbia",
                         ifelse(CMANAME %in% c("Alma", "Baie-Comeau", "Cowansville", "Dolbeau-Mistassini", "Drummondville",
                                               "Granby", "Joliette", "Lachute", "Matane", "Montréal", "Québec", "Rouyn-Noranda", "Rimouski",
                                               "Rivière-du-Loup", "Saguenay", "Saint-Georges", "Saint-Hyacinthe", "Sainte-Marie", "Salaberry-de-Valleyfield",
                                               "Sept-Îles", "Shawinigan", "Sorel-Tracey", "Sherbrooke", "Thetford Mines", "Trois-Rivières",
                                               "Val d'Or", "Victoriaville"), "Quebec", "Other")))

Fig3a[Fig3a$Region=="Quebec",]$Region <- "Québec"

Fig3cola <- c("#8c6bb1","#9ebcda")
Fig3col <- c("#8c6bb1", "grey10", "#9ebcda")
             
ggplot(Fig3a) +
  geom_mark_hull(aes(x = CANrev, 
                   y = cum_rev, 
                    filter = Region == "Québec" | Region == "British Columbia",
                     label = Region, 
                     fill = Region),
                 alpha = 0.25,
                 color = "grey90",
                 radius = unit(5, "mm"),
                 concavity = 2,
                 expand = unit(5, "mm"))+
  scale_fill_manual(name = "Region", values = Fig3cola, guide = FALSE)+
  geom_point(aes(x = CANrev, 
                 y = cum_rev, 
                 size = CMA_pop,
                 color = Region2),
             alpha = 0.9)+
  geom_smooth(aes(x = CANrev, y = cum_rev),
              method = "lm", 
              se = FALSE, 
              color = "black")+
  scale_x_log10(labels = scales::percent_format(accuracy = .1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_size_continuous(name = "Population", 
                        range = c(1,10), 
                        labels = scales::comma_format())+
  scale_color_manual(name = "Region", values = Fig3col, labels = c("British Columbia", "Other", "Québec"))+
  xlab("Percentage of Canada's revenue (log)")+
  ylab("Maximum percentage of CMA revenue earned by \ncensus tracts housing 10% of CMA population")+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
          text=element_text(size=10),
        axis.text = element_text(size = 10),
          panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
          panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
          legend.key = element_blank(),
          panel.background=element_blank(),
          axis.line = element_line(size = .09, color = "grey10"))

Fig3a <- Fig3a %>% 
  mutate(rev = CANrev*1405932782)
cor(Fig3a$cum_rev, Fig3a$rev, method = c("pearson"))

ggplot(Fig3a) +
  geom_smooth(aes(x = CMA_pop, y = cum_rev),method = "lm", se = FALSE, color = "black")+
  geom_point(aes(x = CMA_pop, y = cum_rev, color = province, size = CANrev))+
  scale_x_log10()

CT_pop <- read_csv("data/CTpop.csv")
colnames(CT_pop) <- c("CTUID", "CMANAME", "CTpop")
Canada_daily <- Canada_daily %>% 
  left_join(CT_pop[, c(1,3)], by = "CTUID")

sum(Candaa_daily_red2$Price[Candaa_daily_red2$Status == "R"])


Fig3CMAs2 <- Candaa_daily_red4 %>% 
  filter(CMATYPE %in% c("CMA")) %>% 
  group_by(CMANAME, CTUID) %>% 
  summarise(rev = sum(Price[Status =="R"]), CT_pop = mean(CT_pop), rev_dens = rev/CT_pop) %>% 
  group_by(CMANAME) %>%
  mutate(percrev = rev/sum(rev)) %>% 
  mutate(CTperc_pop = CT_pop/sum(CT_pop, na.rm = TRUE)) %>% 
  arrange(CMANAME, -rev_dens) %>% 
  mutate(new = cumsum(percrev)) %>% 
  mutate(totalrev = sum(rev)) %>% 
  filter(new <= min(new[new > 0.5])) %>% 
  summarise(sum(CTperc_pop, na.rm = TRUE), mean(totalrev))

Fig3CMAs2 <- Fig3CMAs2 %>% 
  mutate(percentage = `mean(totalrev)`/1394012941)

ggplot(data = Fig3CMAs2, aes(x = fct_reorder(CMANAME, `sum(CTperc_pop, na.rm = TRUE)`), y = `sum(CTperc_pop, na.rm = TRUE)`)) +
  geom_point(aes(size = percentage)) +
  geom_segment(aes(x= CMANAME,
                   y = `sum(CTperc_pop, na.rm = TRUE)`,
                   xend = CMANAME,
                   yend = 0), alpha = 0.5)+
  theme_light()+
  labs(x = "City", y = "Percent of Population")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(angle = 90),
        text=element_text(family="Tahoma", size=12),
        legend.title.align = 0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_size_continuous(name = str_wrap("Percentage of Canadian Airbnb revenue", 20), range = c(1, 7), labels = scales::percent_format(accuracy = 1))+
  coord_flip()

Fig3CMAs[Fig3CMAs$CMANAME == "Sudbury",]$CMA_pop <- 164689

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

## figure 4 seasonality ##

longitude <- read_csv("data/Fig9.csv")
longitude[longitude$CMANAME%in%"Montreal",]$CMANAME <- c("Montréal")
longitude[longitude$CMANAME%in%"Trois-Riveres",]$CMANAME <- c("Trois-Rivières")
longitude[longitude$CMANAME%in%"Ottawa",]$CMANAME <- c("Ottawa - Gatineau")
longitude[longitude$CMANAME%in%"Greater Sudbury / Grand Sudbury",]$CMANAME <- c("Sudbury")
longitude[longitude$CMANAME%in%"Quebec",]$CMANAME <- c("Québec")

Fig4 <- Canada_daily %>%
  filter(Date >= "2016-09-01" & Date <= "2018-08-31"& Status == "R"&Housing == TRUE) %>% 
  mutate(month2 = month(Date)) %>%
  group_by(month2,CMANAME) %>% 
  summarise(n = n(), perc = n/sum(n), p = sum(Price), money = p/sum(p))

Fig4 <- Fig4 %>% 
  group_by(CMANAME) %>% 
  mutate(percent = n/sum(n), money = p/sum(p)) %>% 
  left_join(longitude[,c(1, 4)], by = "CMANAME")

Fig4$month3 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(data = Fig4)+
  geom_line(aes(x = month2, y = percent, group = CMANAME, color = Longitude), alpha = 0.5)+
  scale_color_gradientn(colors = rainbow(19))+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Month")+
  ylab("Percent of Activity")+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05),
        panel.grid.minor.y = element_line(size = 0.025))
 

class(Fig4$month2)
Fig4$month3 <- factor(Fig4$month3, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
facet_wrap(~ CMANAME)
  

#Seasonal Decomposition Function 
seasonal_adjust_city <- function(CMA) {
  ts <- Canada_daily %>% 
    filter(Housing == TRUE, Created >= "2016-09-01") %>% 
    group_by(Created) %>% 
    summarise(num = n()) %>% 
    ts(frequency = 365, start = decimal_date(min(.$Created)))
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}
prac <- seasonal_adjust_city("Toronto")

ts2 <- ts
ts <- ts2

seasonal_adjust<- function(daily) {
  ts <- Canada_daily %>% 
    mutate(month = as.yearmon(Date)) %>% 
    group_by(month) %>% 
    summarise(rev = sum(Price[Status == "R"]))
    ts <- ts %>% 
      filter(month<= "Dec 2018" & month>= "Jan 2017") %>% 
      ts(frequency = 12)
ts <- ts[,2]
decompose_ts <- decompose(ts, "multiplicative")
decompose_ts
}

Canada <- seasonal_adjust(Canada_daily)
Canada$seasonal/12


View(decompose_ts$x)

seasonal_adjust_create <- function(daily) {
  ts <- daily %>% 
    filter(Created > "2016-09-01") %>% 
    group_by(Created) %>% 
    summarise(num = n()) %>% 
    ts(frequency = 365, start = decimal_date(min(.$Created)))
  ts <- ts[,2]
  decompose_ts <- decompose(ts, "multiplicative")
  decompose_ts
}


## figure 5 host revenue ##

Fig5a <- Candaa_daily_red4 %>% #determining least/most concentrated cmas for the lines
  filter(CMATYPE == "CMA", Status == "R") %>% 
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
  
Fig5 <- Candaa_daily_red4 %>% # creating percentiles for CMATYPE and joining with least/most cmas above
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

Fig5b <- Candaa_daily_red4 %>% # data for small side charts 
  filter(CMATYPE == "CMA", Status == "R") %>% 
  group_by(CMANAME, Host_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(CMANAME) %>% 
  summarise(one = quantile(rev, c(0.99)), oneperc = sum(rev[rev>one]), `1%` = oneperc/sum(rev),
            five = quantile(rev, c(0.95)), fiveperc = sum(rev[rev>five]), `5%` = fiveperc/sum(rev), 
            ten = quantile(rev, c(0.90)), tenperc = sum(rev[rev>ten]), `10%` = tenperc/sum(rev)) %>% 
  select(CMANAME, `1%`, `5%`, `10%`) %>% 
  gather(`1%`, `5%`, `10%`, key = "percentile", value = "value")
Fig5c <- Fig5b %>% 
  filter(CMANAME %in% c("Montréal", "Abbotsford - Mission"))
   

 filter(CMANAME == CMANAME[which.min(value[percentile == "1%"])] |
          CMANAME == CMANAME[which.max(value[percentile == "1%"])] |
          CMANAME == CMANAME[which.min(value[percentile == "1%" & value > min(value[percentile == "1%"])])] |
          CMANAME == CMANAME[which.max(value[percentile == "1%" & value < max(value[percentile == "1%"])])])

Fig5e <- Fig5d %>% #how  any hosts in each CMA
  group_by(CMANAME) %>% 
  summarise(n())


Fig5col <- c("#59157c", "#9ebcda","#0c316b","#59157c", "#9ebcda","#0c316b", "#59157c", "#9ebcda","#0c316b")
growthcol <- c("#59157c", "#9ebcda","#0c316b")
Fig5c$percentile <- factor(Fig5c$percentile, levels = c('1%', '5%', '10%'))

num2 <- ggplot(data = Fig5c)+ #side plot
  geom_bar(mapping = aes(fill = percentile, x = percentile, y = value), stat = "identity", alpha = 0.65)+
  scale_fill_manual(values = Fig5col)+
  labs(x = "", y = "Percent of Revenue")+
  theme(panel.grid.major.x = element_blank(),
        panel.background=element_blank(),
        axis.title.y = element_blank(),
        text=element_text(size=20),
        panel.grid.major.y = element_line(size = 0.05),
        legend.title = element_blank(),
        panel.grid.minor.y = element_line(size = 0.025))+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'grey20'))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  facet_wrap(~CMANAME)
  
num1 <- ggplot(data = Fig5)+ #main plot
  geom_bar(mapping = aes(fill = percentile, x = CMATYPE, y = value), stat = "identity", position = "dodge", show.legend = FALSE, alpha = 0.65) +
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), lineend = "round", position = position_nudge(x = -0.3), 
               data = . %>% filter(percentile == "1%"))+
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), position = position_nudge(x = 0), 
               data = . %>% filter(percentile == "5%"))+
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), position = position_nudge(x = 0.3), 
               data = . %>% filter(percentile == "10%"))+
  theme(panel.grid.major.x = element_blank(),
        text=element_text(size=20),
        panel.grid.major.y = element_line(size = 0.05),
        panel.background=element_blank(),
        panel.grid.minor.y = element_line(size = 0.025))+
  labs(x = "", y = "Percent of Revenue")+
  scale_fill_manual(values = Fig5col)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  
library(gridExtra)
grid.arrange(num1, num2, ncol = 2) #join the two plots





## figure 6 scatterplot ##

Fig6 <- Candaa_daily_red4 %>% 
  filter(CMATYPE =="CMA" | CMATYPE =="CA") %>% 
  group_by(CMANAME, Property_ID, Listing_Type, CMA_pop) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  group_by(CMANAME) %>% 
  summarise(sum(rev), sum(rev[Listing_Type == "Entire home/apt"]), Population = mean(CMA_pop)) %>% 
  mutate(percEH = `sum(rev[Listing_Type == "Entire home/apt"])`/`sum(rev)`, revCAD = `sum(rev)`*1.2957)

ggplot(data = Fig6, mapping = aes(x = revCAD, y = percEH)) +
  geom_point(aes(size = Population),color = "#0c316b", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Revenue (log)", y = "Percentage of listings\nthat are entire homes")+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_log10(labels = scales::dollar_format())+
  scale_size_continuous(range = c(1,15), labels = scales::comma_format())

## figure 7 version 2 ##
Canada_FREH <- read_csv("data/Canada_FREH.csv")
ly <- Canada_FREH %>% 
  filter(Date == "2019-01-28", FREH == 1)

ly_daily2 <- Candaa_daily_red4 %>%
  group_by(Property_ID, Listing_Type, CMANAME, CMATYPE, CMA_pop) %>% 
  summarise(rev = sum(Price[Status == "R"])) %>% 
  full_join(ly[,c(1,3)], by = "Property_ID") %>% 
  filter(!is.na(CMATYPE)) %>% 
  group_by(CMANAME, CMATYPE, CMA_pop) %>% 
  summarise(`Entire homes` = sum(rev[Listing_Type == "Entire home/apt"])/sum(rev), `Frequently-rented entire homes` = sum(rev[FREH == 1], na.rm = TRUE)/sum(rev, na.rm = TRUE)) %>% 
  rename(Region = CMATYPE)

Fig7 <- c("#59157c" , "#9ebcda")
ly_daily2 %>% 
  filter(CMA_pop>25000) %>% 
  ggplot()+
  geom_point(aes(x = `Entire homes`, y = `Frequently-rented entire homes`, color = Region, size = CMA_pop), alpha = 0.5)+
  scale_color_manual(values = Fig7)+
  geom_smooth(aes(x = `Entire homes`, y = `Frequently-rented entire homes`), method = "lm", se = FALSE, color = "black")+
  scale_size_continuous(name = "Population", range = c(1,20),labels = scales::comma_format())+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  xlab("Percentage of revenue derived from entire-home listings")+
  ylab("Percentage of revenue derived from FREH listings")

ggplot()+
  geom_segment(data = ly_daily2, 
               mapping = aes(y = `Frequently-rented entire homes`, 
                             yend = `Entire homes`, 
                             x = fct_reorder(CMANAME,`Entire homes`), 
                             xend = fct_reorder(CMANAME,`Entire homes`), 
                             linetype = Region))+
  geom_point(data = ly_daily, 
             mapping = aes(y = value, 
                           x = CMANAME,
                           shape = `Percent of Revenue from`))+
  theme(aspect.ratio=1, 
        axis.line.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size = 0.07),
        panel.grid.major.x = element_line(size = 0.015),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(family = "Avenir Light", size = 5),
        panel.border=element_blank(),
        plot.background=element_blank())


## figure 7 radar chart ##

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
  filter(Status == "R" & CMATYPE == "CMA") %>% 
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
  scale_fill_manual(values = c("#a9ebd7", "#b0c6ce", "#938ba1")) +
  scale_color_manual(values = c("#a9ebd7", "#b0c6ce", "#938ba1")) +
  labs(x = "", y = "Percent of revenue")+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size =8),
        axis.line = element_line(size = .09, color = "grey10"))+
  annotate("text", x = 0, 
           y = seq(0, 1, 0.25), 
           label = scales::percent(seq(0, 1, 0.25)),
           size = 2.5) +
  coord_radar()

melt <- melt(Fig7)
melt <- melt %>% 
  filter(variable == "percent_rev")
cast <- dcast(melt, CMANAME ~ Status2, fun.aggregate = sum)



ggplot(data = Fig7, aes(x=fct_reorder(CMANAME, percent_rev), y = percent_rev)) +
  geom_segment(data = cast, aes(x = CMANAME, xend = CMANAME, y = EH, yend = VFREH))+
  geom_point(mapping = aes(color = Status2), size = 5) +
  scale_color_manual(values = c("#a9ebd7", "#b0c6ce", "#938ba1")) +
  labs(x = "", y = "Percent of revenue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## figure 8 maps ##

## figure 9 vacancy rates ##

Fig9 <- read_csv("data/Fig9.csv")

ggplot(Fig9, mapping = aes(x = fct_reorder(CMANAME, Longitude), y = Perc_pop)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "", y = "Percent of population")+
  theme(axis.text.x = element_text(size = 7))+
  ylim(0,1)






############ Other Visuals ###############

### daily resevation scatterplot ###

daily_res2 <- Canada_daily %>% 
  filter(Status == "R") %>% 
  group_by(Date) %>% 
  summarise(n())

ggplot(daily_res2, aes(x=Date, y=`n()`)) +
  geom_point(alpha = 0.5, color = "#b0c6ce") +
  geom_smooth(color = "grey50")+
  labs(x = "Date", y = "Number of reservations")

### stacked bar chart of occupancy rates ###

daily_res <- Canada_daily %>% 
  group_by(Property_ID, Date) %>% 
  group_by(Date) %>% 
  summarise(res = sum(Status == "R"), listings = n() - res) %>% 
  arrange(Date) %>% 
  gather(res,listings, key = "type", value = "value")
ggplot()+
  geom_bar(data = daily_res, mapping = aes(fill = type, x = Date, y =  value), stat = "identity", position = "stack")+
  scale_fill_manual(values = c("grey50","#b0c6ce"))+
  labs(x = "Date", y = "Occupancy Rate")

### alluvial chart ###

par182 <- Canada_daily %>%
  filter(Date == "2018-04-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par181 <- Canada_daily %>%
  filter(Date == "2018-01-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par174 <- Canada_daily %>%
  filter(Date == "2017-10-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par173 <- Canada_daily %>%
  filter(Date == "2017-07-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par172 <- Canada_daily %>%
  filter(Date == "2017-04-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par171 <- Canada_daily %>%
  filter(Date == "2017-01-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par164 <- Canada_daily %>%
  filter(Date == "2016-10-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>% 
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par163 <- Canada_daily %>%
  filter(Date == "2016-07-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par162 <- Canada_daily %>%
  filter(Date == "2016-04-30", CMATYPE == "CMA") %>% 
  group_by(Property_ID, CMANAME, Date) %>% 
  group_by(CMANAME) %>% 
  summarise(num = sum(Price[Status =="R"]), Date = mean(Date)) %>%  
  arrange(-num) %>% 
  mutate(order = row_number()) %>% 
  filter(order <= 50)
par2 <- par164[,c(1,4)] %>% 
  left_join(par171[,c(1,4)], by = "CMANAME") %>% 
  left_join(par172[,c(1,4)], by = "CMANAME") %>% 
  left_join(par173[,c(1,4)], by = "CMANAME") %>% 
  left_join(par174[,c(1,4)], by = "CMANAME") %>%
  left_join(par181[,c(1,4)], by = "CMANAME") %>% 
  left_join(par182[,c(1,4)], by = "CMANAME") %>%
  left_join(popCMA, by= "CMANAME")
colnames(par2) <- c("CMANAME","Oct16", "Jan17", "Apr17", "Jul17", "Oct17", "Jan18", "Apr18", "CMA_pop")
colors <- c("#ea9f80", "#edaa85", "#eeab86", "#f3bd8e", "#f7d497", "#faeaa1", "#f5f1a6", "#e3e8a3",
            "#d5e2a2", "#c8dda1", "#c0d9a2", "#bcd8a2", "#bad7a1", "#bbd7a1", "#bdd9a7", "#bedbbb",
            "#bfdecd", "#bfe0e1", "#bce0ef", "#b0d6f0", "#a3b9dc", "#99a5cf", "#949ac8", "#9197c6",
            "#9397c5", "#9998c6", "#a29ac6", "#ae9cc6", "#bc9fc6", "#caa3c7", "#e5a6c5", "#e7a0b9",
            "#e99c9d", "#e89c9d", "#e99b87")
alluvial(par2[,c(2:8)], freq = par2$CMA_pop, col = colors, gap.width = 0, cw = 0.05, alpha = 0.75, blocks = FALSE, ann = FALSE)

### arc diagram ###

arc <- read_csv("data/arc.csv")
arc <- arc %>% 
  mutate(id = row_number())
arc <- as.data.frame(arc)
arc<- data.frame(arc[,-3], row.names = arc[,3])
arc <- arc %>%
  filter(to %in% c("Toronto", "Vancouver", "Montréal", "Victoria", "Ottawa - Gatineau", "Québec", "Calgary",
                   "Canmore", "Kelowna", "St. Catharines - Niagara", "Hamilton", "Edmonton", "Barrie",
                   "Kitcehener - Cambridge - Waterloo", "Sherbrooke", "London", "Nanaimo", "Collingwood",
                   "Winnipeg", "Halifax")) %>% 
  filter(from %in% c("Toronto", "Vancouver", "Montréal", "Victoria", "Ottawa - Gatineau", "Québec", "Calgary",
                     "Canmore", "Kelowna", "St. Catharines - Niagara", "Hamilton", "Edmonton", "Barrie",
                     "Kitcehener - Cambridge - Waterloo", "Sherbrooke", "London", "Nanaimo", "Collingwood",
                     "Winnipeg", "Halifax"))
arc <- arc %>% 
  group_by(to, from) %>% 
  summarise(n())

order <- c("Nanaimo", "Victoria", "Vancouver", "Kelowna", "Canmore", "Calgary", "Edmonton", "Winnipeg", "London", "Collingwood", 
           "Hamilton", "Barrie", "Toronto", "St. Catharines - Niagara", "Ottawa - Gatineau", "Montréal", "Sherbrooke", "Québec", "Halifax")
grid.col <- rainbow(19)   
df <- data.frame(CMANAME = order, color = grid.col)
arc <- left_join(arc, df, by = c("from" = "CMANAME"))
arc_col <- read_csv("arc_color.csv")
col <- arc_col$col_avg  
col_ordered <- c("#0086FFFF", "#6BFF00FF", "#BCFF00FF", "#00FFD7FF", "#1BFF00FF", "#FF0051FF",
                 "#00D7FFFF", "#FFF200FF", "#00FF86FF", "#BC00FFFF", "#FF0000FF", "#6B00FFFF",
                 "#FF00A1FF", "#FF00F2FF", "#1B00FFFF", "#0036FFFF", "#FFA100FF", "#FF5100FF",
                 "#00FF36FF")
tot_col <- c(col, col_ordered)

arc2 <- arc %>%
  select(-color)

chordDiagram(arc2, annotationTrack = "grid", preAllocateTracks = 1, order = order, grid.col = grid.col, col = col)
circos.trackPlotRegion(track.index = 1, panel.fun = function (x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1]+.1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(-0.15, .5), cex = 1.2)
  circos.axis(h = "top", labels.cex = 0.4, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

# arc diagram with self links
test2 <- Canada_daily %>% 
  filter(Date == "2018-04-30", ML == TRUE, Housing == TRUE, Listing_Type == "Entire home/apt",
         CMANAME %in% c("Toronto", "Vancouver", "Montréal", "Victoria", 
                        "Ottawa - Gatineau", "Québec", "Calgary",
                        "Canmore", "Kelowna", "St. Catharines - Niagara", "Hamilton", "Edmonton", "Barrie",
                        "Kitcehener - Cambridge - Waterloo", "Sherbrooke", "London", "Nanaimo", "Collingwood",
                        "Winnipeg", "Halifax")) %>% 
  group_by(Host_ID, CMANAME) %>% 
  summarise(n())

test3 <- Canada_daily %>% 
  filter(Date == "2018-04-30", ML == TRUE, Housing == TRUE, Listing_Type == "Entire home/apt",
         CMANAME %in% c("Toronto", "Vancouver", "Montréal", "Victoria", 
                        "Ottawa - Gatineau", "Québec", "Calgary",
                        "Canmore", "Kelowna", "St. Catharines - Niagara", "Hamilton", "Edmonton", "Barrie",
                        "Kitcehener - Cambridge - Waterloo", "Sherbrooke", "London", "Nanaimo", "Collingwood",
                        "Winnipeg", "Halifax")) %>% 
  group_by(Host_ID, CMANAME) %>% 
  summarise(n()) %>% 
  group_by(Host_ID) %>% 
  summarise(n = n(), m = sum(`n()`)) %>% 
  filter(n == 1) %>% 
  left_join(test2, by = "Host_ID")

test3 <- test3 %>% 
  group_by(CMANAME) %>% 
  summarise(sum = sum(m)) %>% 
  rename(to = CMANAME, `n()` = sum) %>% 
  mutate(from = to) %>% 
  select(to, from, `n()`)

arc3 <- bind_rows(arc2, test3)


colsss <- df()

arc_col_self <- read_csv("arc_color_self.csv")
col2 <- arc_col_self$col_avg  



### animation of FREH / median rent ratio ### 

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
  filter(CMATYPE =="CMA", (Date == "2016-01-01" | Date == "2016-02-01" | Date == "2016-03-01" | Date == "2016-04-01" | Date == "2016-05-01"
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

lpp <- Canada_daily %>% 
  filter(Housing == TRUE, CMATYPE == "CMA") %>% 
  group_by(Date, CMANAME) %>% 
  summarise(n = n(), mean(CMA_pop)) %>% 
  mutate(lpp = n/`mean(CMA_pop)`)


p <- ggplot(FREH, aes(x = CMANAME, y = ratio, fill = ratio))+
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

anim <- ggplot(lpp, aes(x = CMANAME, y = lpp, fill = lpp))+
  geom_bar(stat = "identity")+
  scale_fill_gradient2(low = "#003366", mid = "#ffffff", high = "#ff0000")+
  ylim(-.02,.1)+
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



 anim_save("lpp.gif", anim)

sum(Canada_property$Housing==TRUE & !is.na(Canada_property$CMATYPE))




