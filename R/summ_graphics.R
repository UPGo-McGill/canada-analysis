#need to add freh2 and ml2 later as filters
Canada_property_red <- Canada_property %>% 
  filter(Created >= "2016-09-01",Housing == TRUE,CMATYPE == "Rural", Listing_Type == "Entire home/apt")
subset <- sample(Canada_property_red$Property_ID, size = 1000, replace = FALSE)
subset_daily <- Canada_daily %>% 
  filter(Property_ID %in% subset)

medcumrev_1 <- Canada_daily %>% 
  filter(Created >= "2016-09-01",Housing == TRUE,CMATYPE == "Rural", Listing_Type == "Entire home/apt") %>% 
  group_by(day_num_r) %>% 
  summarise(med_rev = median(cumrev))

R_diff_1 <- Canada_daily %>% 
  filter(Created >= "2016-09-01", Scraped <= "2018-01-25", Housing == TRUE,CMATYPE == "Rural", Listing_Type == "Entire home/apt") %>% 
  group_by(Property_ID) %>% 
  mutate(r_diff = ifelse(sum(Status == "R") > 0,  
                         which.max(day_num[Status == "R"]) - which.min(day_num[Status == "R"]) + 1, NA)) %>%
  ungroup() %>% 
  summarise(mean(r_diff, na.rm = TRUE), median(lifelength))

winddown_1 <- Canada_daily %>% 
  filter(Created >= "2016-09-01",Housing == TRUE,CMATYPE == "Rural", Listing_Type == "Entire home/apt", Scraped <= "2018-01-25") %>%
  group_by(Property_ID) %>% 
  summarise(deathdiff = ifelse(sum(Status == "R")>0, 
                               Scraped - max(Date[Status == "R"]), NA)) %>% 
  ungroup() %>% 
  summarise(deathdiff = mean(deathdiff, na.rm = TRUE))

rampup <- Canada_daily %>% 
  filter(Created >= "2016-09-01",Housing == TRUE,CMATYPE == "Rural", Listing_Type == "Entire home/apt") %>%
  mutate(diff = day_num - day_num_r) %>% 
  group_by(Property_ID) %>%
  filter(sum(Status == "R")>0) %>% 
  ungroup() %>% 
  summarise(x = mean(diff, na.rm = TRUE))


ggplot(subset_daily, aes(x = day_num_r, y = cumrev))+
  geom_line(aes(group = Property_ID, color = lifelength), alpha = 0.1)+
  geom_line(data= medcumrev_1, mapping = aes(day_num_r, med_rev))+
  geom_rect(aes(xmin = 0, xmax = 825, ymin = 0, ymax = 200000), fill = "blue", alpha =0.05)+
  scale_color_gradientn(colors = rainbow(100))+
  theme_minimal()

geom_rect(aes(xmin = 0, xmax = 130, ymin = -5000, ymax = 0))+
  geom_rect(aes(xmin = 130, xmax = 150, ymin = -5000, ymax = 0), fill = "blue")
  
#----------------------------------------------------------------------------------
  
ppnmed_overall <-Canada_daily %>% 
  filter(Status == "R",Created >= "2016-09-01", Housing == TRUE, !(is.na(CMATYPE)), day_num_r < 730) %>% 
  group_by(day_num_r) %>% 
  summarise(ppn = median(Price), n = n())

occ_overall <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE, day_num_r < 730) %>% 
  group_by(day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())

occ_overall <- occ_overall %>% 
  mutate(occrate2 = occrate * -100)

ggplot()+ #good one drawing both circles!!
  geom_bar(ppnmed_overall, mapping =  aes(day_num_r, ppn), stat = "identity", fill = "#20639b", alpha = 0.7)+
  geom_bar(occ_overall, mapping = aes(day_num_r, occrate2), stat = "identity", fill = "#173F5F", alpha = 0.7)+
  theme_minimal()


ppnmed_overall <- ppnmed_overall %>% 
  slice(1:800)

occ_overall <- occ_overall %>% 
  slice(1:800)

ppnmed_overall <- ppnmed_overall %>% 
  mutate(ppn2 = ppn - 70)

ppnmed_overall <- ppnmed_overall %>% 
  mutate(ppn2 = ppn2 * -1)


empty_bar = 30
to_add <- matrix(NA, empty_bar, ncol(occ_overall))
colnames(to_add) <- colnames(occ_overall)
occ_overall <- rbind(occ_overall, to_add)

empty_bar = 30
to_add2 <- matrix(NA, empty_bar, ncol(ppnmed_overall2))
colnames(to_add2) <- colnames(ppnmed_overall2)
ppnmed_overall2 <- rbind(ppnmed_overall2, to_add2)

  
occ_overall <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Created >= "2016-09-01", Housing == TRUE) %>% 
  group_by(day_num_r) %>% 
  summarise(occrate = sum(Status == "R")/n(), n = n())


occ_overall <- occ_overall %>% 
  mutate(occrate2 = occrate * -100)

occ_overall <- occ_overall %>% 
  mutate(occrate2 = occrate2 - 20)

occ_overall <- occ_overall %>% 
  mutate(occrate2 = occrate2 * -1)


created_overall2 <- Canada_daily %>% 
  filter(!is.na(CMATYPE), Housing == TRUE, Created >= "2016-09-01") %>% 
  mutate(month = month(Created)) %>% 
  group_by(Property_ID, month) %>% 
  summarise(n = n()) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n))

dead_overall2 <- Canada_daily %>% 
  filter(Scraped <= "2019-01-01", Housing == TRUE, Created >= "2016-09-01", !(is.na(CMATYPE))) %>% 
  mutate(month = month(Scraped)) %>% 
  group_by(Property_ID, month) %>% 
  summarise(n = n()) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n))
  
dead_overall2$name <- "Takedowns"
created_overall2$name <- "Creations"
revenue_overall$name <- "Revenue"
revenue_overall <- revenue_overall %>% 
  rename(n = rev, percent = percrev)
  
all <- bind_rows(dead_overall2, created_overall2, revenue_overall)

revenue_overall <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", !(is.na(CMATYPE))) %>% 
  mutate(month = month(Date)) %>% 
  group_by(month) %>%
  summarise(rev = sum(Price[Status == "R"])) %>% 
  mutate(percrev = rev/sum(rev))

ggplot(dead_overall2)+
  geom_density(aes(x = as.Date(newdate2, "%Y-%b"), y = ..scaled..))+
  scale_x_date(breaks = date_breaks(width = "1 month"))

ggplot()+ ## good create/scrape/activity
  geom_area(revenue_overall, mapping = aes(month, percrev), fill = "#f67280", alpha = "0.6", color = "#f67280")+
  geom_area(dead_overall2, mapping = aes(month, percent), fill = "#355c7d", alpha = "0.6", color = "#355c7d")+
  geom_area(created_overall2, mapping = aes(month, percent), fill = "#c06c84", alpha = "0.6", color = "#c06c84")+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_blank()
  )
  coord_polar(start = 0)
  
ggplot(all, aes(x = month, y = percent))+ ## good create/scrape/activity
    geom_line(mapping = aes(color = name))+
    theme_minimal()
 

test <- data.frame(
  month = c(13:50),
  n = 0,
  percent = 0)

dead_overall4 <- rbind(dead_overall4, test2)

dead_overall <- dead_overall %>% 
  slice(1:12)


test2 <- data.frame(
  month = c(13:50),
  n = 0,
  percent = 0)
test2$percent <- as.numeric(test2$percent)
dead_overall4$percent <- as.numeric(dead_overall4$percent)
dead_overall4$month <- as.numeric(dead_overall4$month)

dead_overall4 <- dead_overall4 %>% 
  mutate(percent = replace(.$percent, NA))

seq <- seq(as.Date("2018-01-01", format = "%Y-%m"), as.Date("2018-12-31", format = "%Y-%m"), by = "months")



# Create dataset
data=data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# Set a number of 'empty bar'
empty_bar=10

# Add lines to the initial dataset
to_add = matrix(NA, empty_bar, ncol(data))
colnames(to_add) = colnames(data)
data=rbind(data, to_add)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("green", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

#-------------------------------------------------------------------------------------------------

cumrev <- Canada_daily %>% 
  filter(Housing == TRUE, Created >= "2016-09-01", !(is.na(CMATYPE)), Status == "R") %>% 
  group_by(day_num_r) %>% 
  summarise(median(cumrev))

ggplot(data = ppnmed_overall, mapping = aes(x = day_num_r, y = ppn2))+
  geom_bar(stat = "identity", fill = "#20639b", alpha = 0.5)+
  
  geom_line(data = cumrev, mapping = aes(x = day_num_r, y = `median(cumrev)`/scaleFactor2), alpha = 0.9)+
  scale_y_continuous("Daily Revenue", limits = c(0,40), sec.axis = sec_axis(~.*scaleFactor2, name = "Cumulative Revenue"))+
  xlim(0,720)
  ylim(0,40)

class(cumrev$`median(cumrev)`)  
class(ppnmed_overall$ppn2)

scaleFactor <- max(cumrev$`median(cumrev)`)/max(ppnmed_overall$ppn2)


ggplot()+ #good one drawing both circles!!
  geom_rect(aes(xmin = 0, xmax = 30, ymin = -100, ymax = 30), fill = "#a5c5c3", alpha = 0.5)+
  geom_rect(aes(xmin = 30, xmax = 140, ymin = -100, ymax = 30), fill = "#429f9e", alpha = 0.5)+
  geom_rect(aes(xmin = 140, xmax = 170, ymin = -100, ymax = 30), fill = "#007872", alpha = 0.5)+
  geom_line(data = cumrev, mapping = aes(x = day_num_r, y = `median(cumrev)`/scaleFactor2), alpha = 0.5, color = "#363636")+
  geom_bar(ppnmed_overall, mapping =  aes(day_num_r, ppn2), stat = "identity", fill = "#5970af", alpha = 0.9)+
  geom_bar(occ_overall, mapping = aes(day_num_r, occrate2), stat = "identity", fill = "#374396", alpha = 0.9)+
  scale_y_continuous("Daily Revenue", limits = c(-100,40), sec.axis = sec_axis(~.*scaleFactor2, name = "Cumulative Revenue"))+
  xlim(0,720)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  )




scaleFactor2 = 1000




