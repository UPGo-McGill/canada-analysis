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
  mutate(Region = case_when(
    PRNAME %in% c("British Columbia / Colombie-Britannique") ~ "British Columbia",
    PRNAME %in% c("Ontario") ~ "Ontario", 
    PRNAME %in% c("Quebec / Qu\xe9bec") ~ "Québec",
    PRNAME %in% c("Alberta", "Saskatchewan", "Manitoba") ~ "Prairies",
    TRUE ~ "Atlantic"))


Canada_daily_seas %>% 
  group_by(Region) %>% 
  mutate(seasonal = decompose(ts, "multiplicative"))

## define function 
seasonal_adjust<- function(region, cma) {
  ts <- Canada_daily_seas %>% 
    filter(Region %in% region, 
           CMATYPE %in% cma) %>% 
    mutate(month = as.yearmon(Date)) %>% 
    group_by(month) %>% 
    summarise(rev = sum(Price[Status == "R"]))
  ts <- ts %>% 
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

figure_5_data <- 
  map2(list(atlantic, quebec, ontario, prairies, bc),
     c("Atlantic Provinces", "Québec", "Ontario", "Prairies", "British Columbia"),
     ~as_tibble(
       month = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                          "Sep", "Oct", "Nov", "Dec"),
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                percent = .x$seasonal[1:12]/12,
                Region = .y)) %>% 
       bind_rows()




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


