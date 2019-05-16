
## growth in revenue (Table 1)?
Canada_daily_all %>% 
  ungroup() %>% 
  filter(CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>%  
  group_by(year) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  mutate(CAD = x*1.2986) %>% 
  spread(key = year, value = x) %>% 
  mutate(growth = (LTM-MTM)/MTM)

Canada_daily_all %>% 
  ungroup() %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>%  group_by(year, CMANAME) %>% 
  group_by(year, CMATYPE) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  spread(key = year, value = x) %>% 
  mutate(growth = (LTM-MTM)/MTM)

Canada_daily_all %>% 
  mutate(year = ifelse(Date >= "2018-01-01" & Date <= "2018-12-31", "LTM",
                       ifelse(Date >= "2017-01-01" & Date <= "2017-12-31", "MTM", NA))) %>%  group_by(year, CMANAME) %>% 
  group_by(year) %>% 
  summarise(x = sum(Price[Status == "R"])) %>% 
  spread(key = year, value = x) %>% 
  mutate(growth = (LTM-MTM)/MTM)

##figure 10
fig10a <- read.delim("data/fig10.txt", header = TRUE, fileEncoding = "UTF-16")

library(cancensus)
Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C=c("01")), level = "DA")
cd$GeoUID <- as.numeric(cd$GeoUID)
cma <- get_census("CA16", regions = list(C=c("01")), level = "CMA")

fig10b <- Canada_FREH %>%
  ungroup() %>% 
  filter(Date == "2018-12-31") %>% 
  group_by(winner,CMANAME, PRNAME, CMATYPE) %>% 
  summarise(FREH = sum(FREH)) %>% 
  full_join(cd[,c(1,5:6)], by = c("winner" = "GeoUID")) %>% 
  group_by(CMANAME, PRNAME, CMATYPE) %>% 
  summarise(FREH = sum(FREH),  Dwellings = sum(Dwellings), Population = sum(Population)) %>% 
  mutate(percFREH = FREH/Dwellings) %>% 
  left_join(fig10a[,c(2,5,6)], by = "CMANAME") %>% 
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

ggplot(data = fig10b, mapping = aes(x = percFREH, y = vacrate))+
  geom_jitter(aes(size = Population, color = Region), alpha = .5)+
  scale_color_manual(values = Fig10col, labels = c("Atlantic Provinces", "British Columbia",
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


Fig10col <- c("#0c316b", "#8c6bb1", "grey80", "#2d012d", "#9ebcda")

##growth
growth <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE) %>% 
  group_by(Date, CMATYPE) %>% 
  summarise(Listings = n()) %>% 
  filter(Date<="2019-01-28")

growthcol <- c("#59157c" , "#9ebcda","#0c316b")

growth %>% 
  filter(Date <= "2018-12-31") %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Listings/Listings[Date == "2016-09-01"]*100, color = CMATYPE), lwd = 1.8, show.legend = FALSE)+
  xlab("Date") +
  ylab("Indexed active daily listings (September 1, 2016 = 100)")+
  scale_color_manual(name = "Region", values = growthcol)+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=12),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))


## unindexed
growth %>% 
  filter(Date <= "2018-12-31") %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Listings, color = CMATYPE), lwd = 1.8, show.legend = FALSE)+
  xlab("Date") +
  ylab("Active daily listings")+
  scale_color_manual(name = "Region", values = growthcol)+
  scale_y_continuous(labels=scales::comma_format())+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=12),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))


library(gridExtra)
grid.arrange(a, b, ncol = 2) #join the two plots

