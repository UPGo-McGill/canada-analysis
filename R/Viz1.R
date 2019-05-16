
## Figure 2 - donut chart
Fig2 <- daily %>% 
  filter(Listing_Type != "", CMANAME %in% c("Montréal", "Toronto", "Vancouver")) %>% 
  group_by(CMANAME) %>%
  summarise(sum(as.numeric(Price[Status =="R"])), sum(Status=="R")) %>% 
  mutate(rev = `sum(Status == "R")`/sum(`sum(Status == "R")`)) %>% 
  mutate(count = `sum(as.numeric(Price[Status == "R"]))`/sum(`sum(as.numeric(Price[Status == "R"]))`)) %>% 
  mutate(ymax_rev = cumsum(rev)) %>% 
  mutate(ymin_rev = replace_na(lag(ymax_rev,1),0)) %>% 
  mutate(ymax_count = cumsum(count)) %>% 
  mutate(ymin_count = replace_na(lag(ymax_count,1),0))
  
ggplot(data = Fig2) + 
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

Fig7 <- daily %>% 
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
Fig6 <- daily %>% 
  filter(CMATYPE =="B" | CMATYPE =="D" | CMATYPE =="K" & Date >= "2017-01-01" & Date <= "2017-12-31") %>% 
  group_by(CMANAME) %>% 
  summarise(sum(Price[Status =="R"]), sum(Price[Status =="R" & Listing_Type == "Entire home/apt"]), mean(Population)) %>% 
  mutate(percEH = `sum(Price[Status == "R" & Listing_Type == "Entire home/apt"])`/`sum(Price[Status == "R"])`) %>% 

ggplot(data = Fig6) +
  geom_point(mapping = aes(x = `sum(Price[Status == "R"])`, y = percEH, size = `mean(Population)`, alpha = 0.2)) +
  scale_x_log10()

## Figure 4 - seasonality

Fig4 <- daily %>%
  filter(Date >= "2017-01-01" & Date <= "2017-12-31"& Status == "R" & CMATYPE == "B") %>% 
  mutate(month2 = month(Date)) %>%
  group_by(CMANAME, month2) %>% 
  summarise(n()) %>% 
  group_by(CMANAME) %>% 
  mutate(percent = `n()`/sum(`n()`)) %>% 
  
ggplot(data = Fig4)+
  geom_line(aes(x = month2, y = percent, color = CMANAME))
  
  

 test <- daily_time %>%
  group_by(Date) %>%
  summarise(n(), sum(Price[Status =="R"]))
  

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
Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
ctpop <- get_census("CA16", regions = list(C = c("01")), level = 'CT') #get CTs with population
ctpop <- ctpop %>% 
  select(GeoUID, Population, Dwellings) %>% 
  rename(CTUID = GeoUID, Population_ct = Population)
daily <- daily %>% 
  left_join(ctpop, by = "CTUID")

Fig3 <- daily %>% 
  filter(CMATYPE == "B", Date >= "2017-01-01" & Date <= "2017-12-31") %>% 
  group_by(CMANAME, CTUID) %>% 
  summarise(sum(Price[Status =="R"]), mean(Population.x), mean(Population_ct)) %>% 
  group_by(CMANAME) %>%
  mutate(percrev = `sum(Price[Status == "R"])`/sum(`sum(Price[Status == "R"])`)) %>% 
  mutate(CTperc_pop = `mean(Population_ct)`/sum(`mean(Population_ct)`)) %>% 
  rename(pop = `mean(Population.x)`) %>% 
  arrange(CMANAME, -percrev) %>% 
  mutate(cumsum(percrev)) %>% 
  filter(`cumsum(percrev)` < 0.55) %>% 
  summarise(sum(CTperc_pop), mean(pop)) %>% 
  ggplot()+
  geom_col(aes(x = CMANAME, y = `sum(CTperc_pop)`))
  










## Post raffle processing

# get additional geography attributes
names(raffled) <- c("Property_ID","DAUID","geometry")
DA <- read.dbf("data/DA.dbf", stringsAsFactors = FALSE)
DA$CMANAME <- parse_character(DA$CMANAME, locale = locale(encoding = "UTF-8"))
DA$CMANAME <- as.character(DA$CMANAME)
DA$CSDNAME <- as.character(DA$CSDNAME)
raffled <- raffled %>% 
  left_join(DA[,c(1,3,10,17:20,22)], by = "DAUID")
raffled[raffled$CMANAME%in%"Lloydminster (Saskatchewan part / partie de la Saskatchewan)",]$CMANAME <- c("Lloydminster")
raffled[raffled$CMANAME%in%"Lloydminster (Alberta part / partie de l'Alberta)",]$CMANAME <- c("Lloydminster")
raffled[raffled$CMANAME%in%"Greater Sudbury / Grand Sudbury",]$CMANAME <- c("Greater Sudbury")
raffled[raffled$CMANAME%in%"Hawkesbury (Ontario part / partie de l'Ontario)",]$CMANAME <- c("Hawkesbury")
raffled[raffled$CMANAME%in%"Hawkesbury (partie du Québec / Quebec part)" ,]$CMANAME <- c("Hawkesbury")
raffled[raffled$CMANAME%in%"Ottawa - Gatineau (Ontario part / partie de l'Ontario)",]$CMANAME <- c("Ottawa - Gatineau")
raffled[raffled$CMANAME%in%"Ottawa - Gatineau (partie du Québec / Quebec part)",]$CMANAME <- c("Ottawa - Gatineau")
raffled[raffled$CMANAME%in%"Campbellton (New Brunswick part / partie du Nouveau-Brunswick)",]$CMANAME <- c("Campbellton")
raffled[raffled$CMANAME%in%"Campbellton (partie du Québec / Quebec part)",]$CMANAME <- c("Campbellton")



#get Population
Sys.setenv(CM_API_KEY = 'CensusMapper_477e49d37fc30f1e4a10d6bf9b1dc12d')
cd <- get_census("CA16", regions = list(C = c("01")), level = 'CMA') #get CMAs with population
cd <- cd %>% 
  mutate(prac2 = `Region Name`) %>% 
  mutate(prac2 = str_replace(prac2, "[ ][(].[)]$", "")) %>% 
  mutate(prac2 = str_replace(prac2, "[ ][(]..[)]$", "")) %>% 
  mutate(prac2 = str_replace(prac2, "[ ][(]...[)]$", "")) %>% 
  select(prac2, Population) %>% 
  rename(CMANAME = prac2) %>% 
  arrange(CMANAME) %>% 
  slice(c(-70,-71,-214,-473,-767, -1462,-2098, -2586, -2650, -3010,-3841,-4065,-4066, -4191, -4349:-4351, -4531:-4532, -4563:-4565))
cd[cd$CMANAME=="Lloydminster (Part)",]$CMANAME <- c("Lloydminster")
cd[cd$CMANAME=="Norfolk County",]$CMANAME <- c("Norfolk")
cd[cd$CMANAME=="Campbellton",]$CMANAME <- c("Campbellton") #other cleaning of cancensus file
cd[cd$CMANAME=="Lloydminster",]$Population <- c(34583)
cd[cd$CMANAME=="Nelson",]$Population <- c(18307)
raffled <- raffled %>% 
  left_join(cd, by = "CMANAME")

#join to daily file
daily <- daily %>%  #merge daily file with raffled geog attributes
  left_join(raffled, by = "Property_ID") %>% 
  select(-geometry)

#join to property file
property <- property %>% 
  left_join(raffled, by = "Property_ID") %>% 
  select(-geometry.y)



test <- daily %>% 
  filter(Listing_Type =="Entire home/apt")


daily4 <- daily[1:3000,]
property4 <- property[1:4000,]

daily2 <- read_csv("data/Canada_daily.csv")
