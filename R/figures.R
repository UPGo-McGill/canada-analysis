### PAPER FIGURES

## Figure 1 ####

# Figure created in ArcGIS and Illustrator; not reproducible


## Figure 2 ####

# Code below does not produce legend; need to change show.legend to true once
# and then splice the legend into the final output

figure_2_data <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE) %>% 
  group_by(Date, CMATYPE) %>% 
  summarise(Listings = n())

figure_2_col <- c("#59157c", "#9ebcda", "#0c316b")

figure_2_panel_a <- 
  figure_2_data %>% 
  filter(Date <= "2018-12-31") %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Listings/Listings[Date == "2016-09-01"]*100,
                color = CMATYPE), lwd = 1.8, show.legend = FALSE)+
  xlab("Date") +
  ylab("Indexed active daily listings (September 1, 2016 = 100)")+
  scale_color_manual(name = "Region", values = figure_2_col)+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))

figure_2_panel_b <- 
  figure_2_data %>% 
  filter(Date <= "2018-12-31") %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Listings, color = CMATYPE), lwd = 1.8,
            show.legend = FALSE) +
  xlab("Date") +
  ylab("Active daily listings")+
  scale_color_manual(name = "Region", values = figure_2_col)+
  scale_y_continuous(labels=scales::comma_format())+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))

figure_2 <- grid.arrange(figure_2_panel_a, figure_2_panel_b, ncol = 2)
rm(figure_2_data, figure_2_col, figure_2_panel_a, figure_2_panel_b)


## Figure 3 ####

figure_3_data <-
  Canada_daily %>% 
  filter(CMATYPE == "Rural",
         Date >= "2018-01-01",
         Date <= "2018-12-31",
         Housing == TRUE) %>% 
  left_join(Canada_property[,c("Property_ID", "CSDNAME", "CSDUID")],
            by = "Property_ID") %>%
  group_by(Date, CSDNAME, CSDUID) %>% 
  summarize(n = n(), rev = sum(Price[Status == "R"])) %>% 
  group_by(CSDNAME, CSDUID) %>% 
  summarize(ADL = mean(n), rev = sum(rev)) %>% 
  left_join(csd_units, by = c("CSDUID" = "GeoUID")) %>% 
  mutate(revCAD = 1.2957 * rev)

figure_3_panel_a <- 
  figure_3_data %>% 
  ggplot(aes(x = Dwellings, y = ADL)) +
  geom_point(color = "#0c316b", alpha = 0.5) +
  annotate("text", x = 12950, y = 2063, label = "Whistler", size = 2.5) +
  annotate("text", x = 16000, y = 1000, label = "Prince Edward County",
           size = 2.5) +
  annotate("text", x = 12300, y = 713, label = "Mont-Tremblant", size = 2.5) +
  annotate("text", x = 11450, y = 561, label = "The Blue Mountains",
           size = 2.5) +
  geom_smooth(se = FALSE, color = "black") +
  xlab("Housing units") +
  ylab("Listngs") +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1),
                     limits = c(0, 20500)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     limits = c(0, 2200)) +
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text = element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))

figure_3_panel_b <- 
  figure_3_data %>% 
  ggplot(aes(x = ADL, y = revCAD)) +
  geom_point(color = "#0c316b", alpha = 0.5) +
  annotate("text", x = 1800, y = 87500000, label = "Whistler", size = 2.5) +
  annotate("text", x = 1500, y = 24000000, label = "Prince Edward County", 
           size = 2.5) +
  annotate("text", x = 1150, y = 18000000, label = "Mont-Tremblant",
           size = 2.5) +
  annotate("text", x = 400, y = 30000000, label = "The Blue Mountains",
           size = 2.5) +
  geom_smooth(se = FALSE, color = "black") +
  xlab("Listings") +
  ylab("Revenue") +
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  scale_x_continuous(labels = scales::comma_format(accuracy = 1),
                     limits = c(0, 2250)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(0, 95000000))

figure_3 <- grid.arrange(figure_3_panel_a, figure_3_panel_b, ncol = 2)
rm(figure_3_data, figure_3_panel_a, figure_3_panel_b)


## Figure 4 ####

figure_4_data <- 
  Canada_daily %>% 
  filter(Housing == TRUE, Date >= "2018-01-01", Date <= "2018-12-31",
         !is.na(CMATYPE)) %>% 
  filter(CMATYPE == "CMA") %>% 
  group_by(CMANAME, CTUID, PRNAME, CMA_pop) %>% 
  summarize(rev = sum(Price[Status =="R"]), CT_pop = mean(CT_pop),
            rev_dens = rev / CT_pop) %>% 
  group_by(CMANAME, PRNAME, CMA_pop) %>%
  mutate(perc_rev = rev / sum(rev),
         CT_perc_pop = CT_pop / sum(CT_pop, na.rm = TRUE)) %>% 
  arrange(CMANAME, -rev_dens) %>% 
  mutate(cumsum_perc_rev = cumsum(perc_rev),
         cumsum_CT_perc_pop = cumsum(CT_perc_pop)) %>% 
  filter(cumsum_CT_perc_pop >= 0.1) %>% 
  summarize(cum_rev = first(cumsum_perc_rev),
            CAN_rev = sum(rev) / 1405932782) %>% 
  mutate(Region = ifelse(
    PRNAME %in% c("British Columbia / Colombie-Britannique"),
    "British Columbia",
    ifelse(
      CMANAME %in% c("Alma", "Baie-Comeau", "Cowansville", "Dolbeau-Mistassini",
                     "Drummondville", "Granby", "Joliette", "Lachute", "Matane",
                     "Montréal", "Québec", "Rouyn-Noranda", "Rimouski", 
                     "Rivière-du-Loup", "Saguenay", "Saint-Georges", 
                     "Saint-Hyacinthe", "Sainte-Marie", 
                     "Salaberry-de-Valleyfield", "Sept-Îles", "Shawinigan",
                     "Sorel-Tracey", "Sherbrooke", "Thetford Mines",
                     "Trois-Rivières", "Val d'Or", "Victoriaville"),
      "Québec", "Other")))

figure_4_hull_col <- c("#8c6bb1","#9ebcda")
figure_4_point_col <- c("#8c6bb1", "grey10", "#9ebcda")

figure_4 <- 
  figure_4_data %>% 
  ggplot() +
  geom_mark_hull(aes(x = CAN_rev, 
                     y = cum_rev, 
                     filter = Region == "Québec" | Region == "British Columbia",
                     label = Region, 
                     fill = Region),
                 alpha = 0.25,
                 color = "grey90",
                 label.fontsize = 8) +
  scale_fill_manual(name = "Region", values = figure_4_hull_col,
                    guide = FALSE) +
  geom_point(aes(x = CAN_rev, 
                 y = cum_rev, 
                 size = CMA_pop,
                 color = Region),
             alpha = 0.9) +
  geom_smooth(aes(x = CAN_rev, y = cum_rev),
              method = "lm", 
              se = FALSE, 
              color = "black") +
  scale_x_log10(labels = scales::percent_format(accuracy = .1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(name = "Population", 
                        range = c(1, 10), 
                        labels = scales::comma_format()) +
  scale_color_manual(name = "Region", values = figure_4_point_col,
                     labels = c("British Columbia", "Other", "Québec")) +
  xlab("Percentage of Canada's revenue (log)") +
  ylab("Maximum percentage of CMA revenue earned by
       census tracts housing 10% of CMA population") +
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))

rm(figure_4_data, figure_4_hull_col, figure_4_point_col)



#Visual 5 - seasonality
#see SeasonalAdjustment file

#Visual 6 - host percentiles
Fig6_CMA <- Candaa_daily_red4 %>% #determining least/most concentrated cmas for the lines
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

Fig6 <- Candaa_daily_red4 %>% # creating percentiles for CMATYPE and joining with least/most cmas above
  group_by(CMATYPE, Host_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(CMATYPE) %>% 
  summarise(one = quantile(rev, c(0.99)), oneperc = sum(rev[rev>one]), `1%` = oneperc/sum(rev),
            five = quantile(rev, c(0.95)), fiveperc = sum(rev[rev>five]), `5%` = fiveperc/sum(rev), 
            ten = quantile(rev, c(0.90)), tenperc = sum(rev[rev>ten]), `10%` = tenperc/sum(rev)) %>%
  select(CMATYPE, `1%`, `5%`, `10%`) %>% 
  gather(`1%`, `5%`, `10%`, key = "percentile", value = "value") %>% 
  left_join(Fig6_CMA)
Fig6$percentile <- factor(Fig6$percentile, levels = c('1%', '5%', '10%'))

Fig6_CMA2 <- Candaa_daily_red4 %>% # data for small side charts 
  filter(CMATYPE == "CMA", Status == "R") %>% 
  group_by(CMANAME, Host_ID) %>% 
  summarise(rev = sum(Price)) %>% 
  group_by(CMANAME) %>% 
  summarise(one = quantile(rev, c(0.99)), oneperc = sum(rev[rev>one]), `1%` = oneperc/sum(rev),
            five = quantile(rev, c(0.95)), fiveperc = sum(rev[rev>five]), `5%` = fiveperc/sum(rev), 
            ten = quantile(rev, c(0.90)), tenperc = sum(rev[rev>ten]), `10%` = tenperc/sum(rev)) %>% 
  select(CMANAME, `1%`, `5%`, `10%`) %>% 
  gather(`1%`, `5%`, `10%`, key = "percentile", value = "value") %>% 
  filter(CMANAME %in% c("Montréal", "Abbotsford - Mission"))
Fig6_CMA2$percentile <- factor(Fig6_CMA2$percentile, levels = c('1%', '5%', '10%'))

Fig6col <- c("#59157c", "#9ebcda","#0c316b","#59157c", "#9ebcda","#0c316b", "#59157c", "#9ebcda","#0c316b")

num2 <- ggplot(data = Fig6_CMA2)+ #side plot
  geom_bar(mapping = aes(fill = percentile, x = percentile, y = value), stat = "identity", alpha = 0.65)+
  scale_fill_manual(values = Fig6col)+
  labs(x = "", y = "Percent of Revenue")+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'grey20'))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  facet_wrap(~CMANAME)

num1 <- ggplot(data = Fig6)+ #main plot
  geom_bar(mapping = aes(fill = percentile, x = CMATYPE, y = value), stat = "identity", position = "dodge", show.legend = FALSE, alpha = 0.65) +
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), lineend = "round", position = position_nudge(x = -0.3), 
               data = . %>% filter(percentile == "1%"))+
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), position = position_nudge(x = 0), 
               data = . %>% filter(percentile == "5%"))+
  geom_segment(mapping = aes(x = CMATYPE, xend = CMATYPE, y = max, yend = min), position = position_nudge(x = 0.3), 
               data = . %>% filter(percentile == "10%"))+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  labs(x = "", y = "Percent of Revenue")+
  scale_fill_manual(values = Fig6col)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

grid.arrange(num1, num2, ncol = 2)
library(gridExtra)
#Visual 7 - multilistngs
arc <- read_csv("data/arc.csv") #load matrix
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
order <- c("Nanaimo", "Victoria", "Vancouver", "Kelowna", #order to draw the cities in (by longitude here)
           "Canmore", "Calgary", "Edmonton", "Winnipeg", "London", 
           "Collingwood", "Hamilton", "Barrie", "Toronto", 
           "St. Catharines - Niagara", "Ottawa - Gatineau", 
           "Montréal", "Sherbrooke", "Québec", "Halifax")
grid.col <- rainbow(19) #color of circle border 
arc_col <- read_csv("arc_color.csv") #load csv with average colors
col <- arc_col$col_avg #take average color and turn it into a vector

chordDiagram(arc, annotationTrack = "grid", preAllocateTracks = 1, order = order, grid.col = grid.col, col = col)
circos.trackPlotRegion(track.index = 1, panel.fun = function (x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1]+.1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(-0.15, .5), cex = 1.2)
  circos.axis(h = "top", labels.cex = 0.4, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

# arc diagram with self links
test2 <- Canada_daily %>% #number of ML per Host_ID per city 
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
  summarise(n()) %>% #number of ML per host
  group_by(Host_ID) %>% 
  summarise(n = n(), m = sum(`n()`)) %>% #n = number of cities per host, m = total number of multilistings
  filter(n == 1) %>% #filter to only hosts with ML within one city
  left_join(test2, by = "Host_ID") #join with test 2 to get CMANAME again (also reincludes total ML per host (n()) but doesn't need to)

test4 <- test3 %>% 
  group_by(CMANAME) %>% 
  summarise(sum = sum(m)) %>% #sum total ML per city
  rename(to = CMANAME, `n()` = sum) %>% #rename columns so that they match the convention above and can be joined to cross-city ML
  mutate(from = to) %>% 
  select(to, from, `n()`)

arc3 <- bind_rows(arc2, test4)
arc_col_self <- read_csv("arc_color_self.csv")
col2 <- arc_col_self$col_avg

chordDiagram(arc3, annotationTrack = "grid", preAllocateTracks = 1, order = order, grid.col = grid.col, col = col2)
circos.trackPlotRegion(track.index = 1, panel.fun = function (x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1]+.1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(-0.15, .5), cex = 1.2)
  circos.axis(h = "top", labels.cex = 0.4, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

#Visual 8 - percent revenue entire homes
Fig8 <- Candaa_daily_red4 %>% 
  filter(CMATYPE =="CMA" | CMATYPE =="CA") %>% 
  group_by(CMANAME, Property_ID, Listing_Type, CMA_pop) %>% 
  summarise(n = n(), rev = sum(Price[Status == "R"])) %>% 
  group_by(CMANAME) %>% 
  summarise(sum(rev), sum(rev[Listing_Type == "Entire home/apt"]), Population = mean(CMA_pop)) %>% 
  mutate(percEH = `sum(rev[Listing_Type == "Entire home/apt"])`/`sum(rev)`, revCAD = `sum(rev)`*1.2957)

ggplot(data = Fig8, mapping = aes(x = revCAD, y = percEH)) +
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

#Visual 9 - percent of rev from EH vs percent of rev from  FREH
Canada_FREH <- read_csv("data/Canada_FREH.csv")

FREH_2018 <- Canada_FREH %>% 
  filter(Date == "2018-12-31", FREH == 1)

Fig9 <- Candaa_daily_red4 %>%
  group_by(Property_ID, Listing_Type, CMANAME, CMATYPE, CMA_pop) %>% 
  summarise(rev = sum(Price[Status == "R"])) %>% 
  left_join(FREH_2018[,c(1,3)], by = "Property_ID") %>% 
  group_by(CMANAME, CMATYPE, CMA_pop) %>% 
  summarise(`Entire homes` = sum(rev[Listing_Type == "Entire home/apt"])/sum(rev), 
            `Frequently-rented entire homes` = sum(rev[FREH == 1], na.rm = TRUE)/sum(rev, na.rm = TRUE)) %>% 
  rename(Region = CMATYPE)

Fig9col <- c("#59157c", "#9ebcda")

Fig9 %>% 
  filter(CMA_pop>25000) %>% 
  ggplot()+
  geom_point(aes(x = `Entire homes`, y = `Frequently-rented entire homes`, color = Region, size = CMA_pop), alpha = 0.5)+
  scale_color_manual(values = Fig9col)+
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

#Visual 10 - map

#Visual 11 - vacancy rates
vacrates <- read.delim("data/fig10.txt", header = TRUE, fileEncoding = "UTF-16")
Canada_FREH <- read_csv("data/Canada_FREH.csv")
Canada_FREH <- Canada_FREH %>% 
  left_join(Canada_property[,c(1,5,19,20,21,22,24,25,26)])

cd <- get_census("CA16", regions = list(C=c("01")), level = "DA")
cd$GeoUID <- as.numeric(cd$GeoUID)
cma <- get_census("CA16", regions = list(C=c("01")), level = "CMA")

fig11 <- Canada_FREH %>%
  ungroup() %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(winner,CMANAME, PRNAME, CMATYPE) %>% 
  summarise(FREH = sum(FREH)) %>% 
  full_join(cd[,c(1,5:6)], by = c("winner" = "GeoUID")) %>% 
  group_by(CMANAME, PRNAME, CMATYPE) %>% 
  summarise(FREH = sum(FREH),  Dwellings = sum(Dwellings), Population = sum(Population)) %>% 
  mutate(percFREH = FREH/Dwellings) %>% 
  left_join(vacrates[,c(2,5,6)], by = "CMANAME") %>% 
  filter(e != "d") %>%
  rename(vacrate = e) %>% 
  mutate(ratio = percFREH/vacrate) %>% 
  mutate(Region = ifelse(PRNAME %in% c("British Columbia / Colombie-Britannique"), "British Columbia",
                         ifelse(PRNAME %in% c("Ontario"), "Ontario", 
                                ifelse(CMANAME %in% c("Alma", "Baie-Comeau", "Cowansville", "Dolbeau-Mistassini", "Drummondville",
                                                      "Granby", "Joliette", "Lachute", "Matane", "Montréal", "Québec", "Rouyn-Noranda", "Rimouski",
                                                      "Rivière-du-Loup", "Saguenay", "Saint-Georges", "Saint-Hyacinthe", "Sainte-Marie", "Salaberry-de-Valleyfield",
                                                      "Sept-Îles", "Shawinigan", "Sorel-Tracey", "Sherbrooke", "Thetford Mines", "Trois-Rivières",
                                                      "Val d'Or", "Victoriaville"), "Quebec",
                                       ifelse(PRNAME %in% c("Alberta", "Saskatchewan", "Manitoba"), "Prairies","Atlantic Canada")))))

Fig11col <- c("#0c316b", "#8c6bb1", "grey80", "#2d012d", "#9ebcda")

ggplot(data = fig11, mapping = aes(x = percFREH, y = vacrate))+
  geom_jitter(aes(size = Population, color = Region), alpha = .5)+
  scale_color_manual(values = Fig11col, labels = c("Atlantic Provinces", "British Columbia",
                                                   "Ontario", "Prairies", "Québec"))+
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_size_continuous(range = c(1,20), labels = scales::comma_format())+
  scale_y_continuous(limits=c(0,.2), labels = scales::percent_format(accuracy = 1))+
  scale_x_log10(labels = scales::percent_format(accuracy = .1))+
  xlab("Percentage of housing units that are FREH listings (log)")+
  ylab("Vacancy rate")+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))
