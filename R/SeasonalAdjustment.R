library(zoo)
library(gridExtra)

## create smallest possible version of the daily file
Canada_daily_seas <- Canada_daily %>% 
  ungroup() %>% 
  filter(Housing == TRUE, 
         !is.na(CMATYPE), 
         Date <="2018-12-31", 
         Date >= "2017-01-01")

## add Atlantic / other column
Canada_daily_seas <- Canada_daily_seas %>% 
  mutate(Region = ifelse(PRNAME %in% c("British Columbia / Colombie-Britannique"), "British Columbia",
                         ifelse(PRNAME %in% c("Ontario"), "Ontario", 
                                ifelse(PRNAME %in% c("Quebec / Qu\xe9bec"), "Québec",
                                       ifelse(PRNAME %in% c("Alberta", "Saskatchewan", "Manitoba"), "Prairies","Atlantic")))))

## define function 
seasonal_adjust<- function(region, cma) {
  ts <- Canada_daily_seas %>% 
    filter(Region %in% region, 
           CMATYPE %in% cma) %>% 
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

## Atlantic
atlantic <- seasonal_adjust(region = c("Atlantic"), cma = c("CMA", "CA", "Rural"))

## Prairies
prairies <- seasonal_adjust(region = c("Prairies"), cma = c("CMA", "CA", "Rural"))

## BC
bc <- seasonal_adjust(region = c("British Columbia"), cma = c("CMA", "CA", "Rural"))

## Ontario
ontario <- seasonal_adjust(region = c("Ontario"), cma = c("CMA", "CA", "Rural"))

## QC
quebec <- seasonal_adjust(region = c("Québec"), cma = c("CMA", "CA", "Rural"))

## Visual
atlanticviz <- atlantic$seasonal[1:12]/12
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
atlanticviz2 <- as.data.frame(month, atlanticviz)
atlanticviz <- atlanticviz2 %>% 
  mutate(percent = atlanticviz) %>%
  mutate(Region = "Atlantic Provinces")
rm(atlanticviz2)

ontarioviz <- ontario$seasonal[1:12]/12
ontarioviz2 <- as.data.frame(month, ontarioviz)
ontarioviz <- ontarioviz2 %>% 
  mutate(percent = ontarioviz) %>%
  mutate(Region = "Ontario")
rm(ontarioviz2)

prairiesviz <- prairies$seasonal[1:12]/12
prairiesviz2 <- as.data.frame(month, prairiesviz)
prairiesviz <- prairiesviz2 %>% 
  mutate(percent = prairiesviz) %>%
  mutate(Region = "Prairies")
rm(prairiesviz2)

bcviz <- bc$seasonal[1:12]/12
bcviz2 <- as.data.frame(month, bcviz)
bcviz <- bcviz2 %>% 
  mutate(percent = bcviz) %>%
  mutate(Region = "British Columbia")
rm(bcviz2)

quebecviz <- quebec$seasonal[1:12]/12
quebecviz2 <- as.data.frame(month, quebecviz)
quebecviz <- quebecviz2 %>% 
  mutate(percent = quebecviz) %>%
  mutate(Region = "Québec")
rm(quebecviz2)


atlanticviz <- rbind(atlanticviz, bcviz, prairiesviz, quebecviz, ontarioviz)
atlanticviz$month <- factor(atlanticviz$month, levels = c("Jan", "Feb", "Mar", 
                                                          "Apr", "May", "Jun", 
                                                          "Jul", "Aug", "Sep", 
                                                          "Oct", "Nov", "Dec"))



seas_col <- c("#0c316b", "#8c6bb1", "grey80", "#2d012d", "#9ebcda")
ggplot()+
  geom_line(data = atlanticviz,
            mapping = aes(x = month, 
                          y = percent, 
                          color = Region, 
                          group = Region), lwd = 1.8, alpha = 0.8)+
  theme(panel.grid.major.x = element_line(size = 0.05, color = "grey80"),
        text=element_text(size=10),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_line(size = 0.05, color = "grey80"),
        panel.grid.minor.y = element_line(size = 0.025, color = "grey80"),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        panel.background=element_blank(),
        axis.line = element_line(size = .09, color = "grey10"))+
  xlab("Month")+
  ylab("Percentage of revenue")+
  scale_color_manual(name = "", values = seas_col)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


