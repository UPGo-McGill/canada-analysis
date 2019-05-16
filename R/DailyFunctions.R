##GO THROUGH AND RECOMMENT THINGS
## FIX ACTIVEEHREV TABLE TO DATE

#select points within Tofino's DAUIDs and subset them from property file
Tofino_prop <- subset(Canada_property, Canada_property$DAUID %in% c(59230087:59230090))
Tofino_daily <- Canada_daily[Canada_daily$Property_I %in% Tofino_prop$Property_I,]

#Setup
setup <- function(property, daily) {
  
  library(tidyr)
  library(zoo)
  library(dplyr)
  daily_prop <- merge(daily, property, by = c("Property_I"), incomparables = NA) #merge daily and property files
  daily_prop$monthYear <- as.Date(as.yearmon(daily_prop$Date)) #create monthYear column
  daily_prop$Year <- format(as.Date(daily_prop$Date, format="%d/%m/%Y"),"%Y") #create Year column
  
  daily_prop[daily_prop$Date < "2015-05-01",]$TMcode <- "1"
  daily_prop[daily_prop$Date < "2016-05-01" & daily_prop$Date > "2015-04-30",]$TMcode <- "2"
  daily_prop[daily_prop$Date < "2017-05-01" & daily_prop$Date > "2016-04-30",]$TMcode <- "3"
  daily_prop[daily_prop$Date < "2018-05-01" & daily_prop$Date > "2017-04-30",]$TMcode <- "4"
  daily_prop
}


# Revenue Tables
active_revenue <- function(daily_prop) {
  rev_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R",], Date, City), sum(Price)) #tidy form of revenue table for active listings per month per city
  rev_spread <- spread(data = rev_tidy,  #spread (Excel) form of revenue table for active listings per month per city
                       key = Date,
                       value = `sum(Price)`,
                       drop=FALSE)
  list("tidy" = rev_tidy, "spread" = rev_spread)
}


#Number of reservations per night in each city
reservations <- function(daily_prop){
  r_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R",], Date,City, Property_I), n()) #summarize daily_prop by date, city, and Property_ID to get count of Rs per PID
  r_tidy <- aggregate(r_tidy_PID$`n()`, list(r_tidy_PID$Date, r_tidy_PID$City), length) #tidy form of R counts for R>0 per day per city
  names(r_tidy) <- c("Date", "City", "R_gt_0") #rename columns
  r_spread <- spread(data = r_tidy, #spread (Excel) form of R counts (R>0 for any given month) per month per city
                     key = Date,
                     value = 'R_gt_0',
                     drop=FALSE)
  list("tidy" = r_tidy, "spread" = r_spread)
}


#Number of active listings per night in each city - IS THIS OKAY, BECAUSE WE ALREADY SCRAPED IT TO MATCH THE LSD SO CAN'T I JUST COUNT ALL THE LISTINGS IN EXISTENCE?
active_listings <- function(daily_prop){
  
  r_tidy <- aggregate(daily_prop, list(daily_prop$Date, daily_prop$City), length) 
  names(r_tidy) <- c("Date", "City", "R_gt_0") #rename columns
  r_spread <- spread(data = r_tidy,
                     key = Date,
                     value = 'R_gt_0',
                     drop=FALSE)
  list("tidy" = r_tidy, "spread" = r_spread)
}


##OLD
#Active A+R > 0 Tables
ar_tidy_PID <- summarise(group_by(daily_prop[!daily_prop$Status=="B",], monthYear,City, Property_I), n()) #summarize daily_prop by monthYear, city, and Property_ID to get count of A+R per PID
ar_tidy <- aggregate(ar_tidy_PID$`n()`, list(ar_tidy_PID$monthYear, ar_tidy_PID$City), length) #tidy form of A+R counts for R>0 per month per city
names(ar_tidy) <- c("Date", "City", "R_gt_0") #rename columns
ar_spread <- spread(data = ar_tidy, #spread (Excel) form of A+R counts per month per city
                    key = Date,
                    value = 'R_gt_0',
                    drop=FALSE)


#Active EH Listings Tables
eh_listings <- function(daily_prop) {
   eh_listings_tidy <- aggregate(daily_prop, list(daily_prop$Date, daily_prop$City), length) #tidy form of EH counts
  names(eh_listings_tidy) <- c("Date", "City", "R_gt_0") #rename columns
  eh_listings_spread <- spread(data = eh_listings_tidy, #spread (Excel) form of EH R counts (R>0 for any given month) per month per city
                               key = Date,
                               value = 'R_gt_0',
                               drop=FALSE)
  eh_listings <- list("tidy" = eh_listings_tidy, "spread" = eh_listings_spread)
  rm(eh_listings_tidy_PID)
}


# Active (R>0 in a given month) EH Revenue Tables 
eh_revenue <- function(daily_prop) {
  eh_rev_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], monthYear, City), sum(Price)) #tidy form of EH revenue table for active listings per month per city
  eh_rev_spread <- spread(data = eh_rev_tidy,  #spread (Excel) form of EH revenue table for active listings per month per city
                          key = monthYear,
                          value = `sum(Price)`,
                          drop=FALSE)
  eh_revenue <- list("tidy" = eh_rev_tidy, "spread" = eh_rev_spread)
}

#___________________________________________________________________________________________________________________________________________

$R
rtest1 <- summarise(group_by(subset[subset$Status=="R"], Date, Property_I), n())
rtest1_spread <- spread(rtest1, 
                       key = Date, 
                       value = 'n()',
                       drop = FALSE)

rnoprop <- rtest1_spread[,-1]
rnoprop <- rnoprop[,1:50]
rtest4 <- t(apply(rnoprop, 1, FUN = function(x) rollsum(x, 25, na.rm = TRUE, align = "right")))
rtest5 <- as.data.frame(rtest4)
rcols <- c(seq.Date(as.Date("2014-10-01"),as.Date("2014-10-25"), by = 1))
colnames(rtest5) <- cols
rtest6 <- cbind(rtest1_spread$Property_I, rtest5)
colnames(rtest6)[1] <- "Property_I"



#A+R
atest1 <- summarise(group_by(subset[!subset$Status=="B"], Date, Property_I), n())
atest1_spread <- spread(atest1, 
                       key = Date, 
                       value = 'n()',
                       drop = FALSE)

anoprop <- atest1_spread[,-1]
anoprop <- anoprop[,1:50]
atest4 <- t(apply(anoprop, 1, FUN = function(x) rollsum(x, 25, na.rm = TRUE, align = "right")))
atest5 <- as.data.frame(atest4)
acols <- c(seq.Date(as.Date("2014-10-01"),as.Date("2014-10-25"), by = 1))
colnames(atest5) <- acols
atest6 <- cbind(atest1_spread$Property_I, atest5)
colnames(atest6)[1] <- "Property_I"


#make the data.frames the same size
atest7 <- atest6[atest6$Property_I %in% rtest6$Property_I,] 


FREH <-  atest7>15 & rtest6 > 8
FREH <- apply(FREH[,2:27], 2, as.numeric)


#EXTRA FREH ATTEMPTS

library(data.table)
setDT(daily_prop)     # converts test to a data.table in place
setkey(daily_prop, Property_I, Date)
daily_prop[,newcol:=as.numeric(runSum(Price,3)), by=Property_I]
View(daily_prop)









#FREH counts
freh_listings <- function(daily_prop) {
  freh_r_pid <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #get number of R EH per PID per TMcode
  freh_ar_pid <- summarise(group_by(daily_prop[!daily_prop$Status=="B"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #get number of A+R EH per PID per TMcode
  
  freh_a_ar <- merge(freh_r_pid, freh_ar_pid, by = c("Property_I", "TMcode")) #merge R and AR counts into one file
  names(freh_a_ar) <- c("Property_I", "TMcode", "City", "R","City", "A_R") #rename columns
  
  freh_tidy <- aggregate(freh_a_ar[freh_a_ar$R>59 &freh_a_ar$A_R>119,]$TMcode, list(freh_a_ar[freh_a_ar$R>59 &freh_a_ar$A_R>119,]$TMcode, freh_a_ar[freh_a_ar$R>59 &freh_a_ar$A_R>119,]$City), length) #tidy count of FREH listings per month per city
  names(freh_tidy) <- c("TMcode", "City", "FREH") #rename columns
  freh_spread <- spread(data = freh_tidy, #spread (Excel) form of FREH counts per month per city
                        key = TMcode,
                        value = 'FREH',
                        drop=FALSE)
  freh_listings <- list("tidy" = freh_tidy, "spread" = freh_spread)
}


#VFREH counts
vfreh_listings <- function(daily_prop) {
  vfreh_r_pid <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #get number of R EH per PID per year (same exact output as first step in FREH counts)
  vfreh_ar_pid <- summarise(group_by(daily_prop[!daily_prop$Status=="B"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n())#get number of A+R EH per PID pe year (same exact output as second step in FREH counts)
  
  vfreh_a_ar <- merge(vfreh_r_pid, vfreh_ar_pid, by = c("Property_I", "TMcode")) #merge R and AR counts into one file (same exact output as third step in FREH counts
  names(vfreh_a_ar) <- c("Property_I", "TMcode", "City", "R","City", "A_R") #rename columns (same exact output as fourth step in FREH counts)
  
  vfreh_tidy <- aggregate(vfreh_a_ar[vfreh_a_ar$R>119 &vfreh_a_ar$A_R>239,]$R, list(vfreh_a_ar[vfreh_a_ar$R>119 &vfreh_a_ar$A_R>239,]$TMcode, vfreh_a_ar[vfreh_a_ar$R>119 &vfreh_a_ar$A_R>239,]$City), length) #tidy count of VFREH listings per month per city
  names(vfreh_tidy) <- c("TMcode", "City", "FREH") #rename columns
  vfreh_spread <- spread(data = vfreh_tidy, #spread (Excel) form of VFREH counts per month per city
                         key = TMcode,
                         value = 'FREH',
                         drop=FALSE)
  
  vfreh_listings <- list("tidy" = vfreh_tidy, "spread" = vfreh_spread)
}


#FREH Revenue
freh_revenue <- function(daily_prop) {
  freh_rev_pid <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), sum(Price)) #get sum of price for all reservations per property per TMcode
  freh_rev2 <- merge(freh_rev_pid, freh_a_ar, by = c("Property_I", "TMcode")) #merge sum of price with R and AR counts from previous step
  names(freh_rev2) <- c("Property_I", "TMcode", "City", "Price", "City","R","City", "A_R") #rename columns
  freh_rev_tidy <- aggregate(freh_rev2[freh_rev2$R>59 &freh_rev2$A_R>119,]$Price, list(freh_rev2[freh_rev2$R>59 &freh_rev2$A_R>119,]$TMcode, freh_rev2[freh_rev2$R>59 &freh_rev2$A_R>119,]$City), sum) #tidy sum of revenue of FREH listings per month per city
  names(freh_rev_tidy) <- c("TMcode","City", "Price")
  freh_rev_spread <- spread(data = freh_rev_tidy, #spread (Excel) form of revenue of FREH per month per city
                            key = TMcode,
                            value = 'Price',
                            drop=FALSE)
  freh_revenue <- list("tidy" = freh_rev_tidy, "spread" = freh_rev_spread)
}


#VFREH Revenue
vfreh_revenue <- function(daily_prop) {
  vfreh_rev_pid <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), sum(Price)) #get sum of price for all reservations per property per TMcode (same as FREH)
  vfreh_rev2 <- merge(vfreh_rev_pid, freh_a_ar, by = c("Property_I", "TMcode")) #merge sum of price with R and AR counts from previous step (same as FREH)
  names(vfreh_rev2) <- c("Property_I", "TMcode", "City", "Price", "City","R","City", "A_R") #rename columns
  vfreh_rev_tidy <- aggregate(vfreh_rev2[vfreh_rev2$R>119 &vfreh_rev2$A_R>239,]$Price, list(vfreh_rev2[vfreh_rev2$R>119 &vfreh_rev2$A_R>239,]$TMcode, vfreh_rev2[vfreh_rev2$R>119 &vfreh_rev2$A_R>239,]$City), sum) #tidy sum of revenue of VFREH listings per month per city
  names(vfreh_rev_tidy) <- c("TMcode","City", "Price") #rename columns
  vfreh_rev_spread <- spread(data = vfreh_rev_tidy, #spread (Excel) form of revenue of VFREH listings per month per city
                             key = TMcode,
                             value = 'Price',
                             drop=FALSE)
  vfreh_revenue <- list("tidy" = vfreh_rev_tidy, "spread" = vfreh_rev_spread)
}

#run through all of monthly functions for Tofino NEED TO BE CHANGED TO NEW FUNCTION NAMES
Tofino_daily_prop <- setup(Tofino_prop, Tofino_daily)
Tofino_rev_month <- revenue_month(Tofino_daily_prop)
Tofino_active_r <- active_r(Tofino_daily_prop)
Tofino_eh_listings <- eh_listings(Tofino_daily_prop)
Tofino_eh_rev <- eh_revenue(Tofino_daily_prop)
Tofino_freh_listings <- freh_listings(Tofino_daily_prop)
Tofino_vfreh_listings <- vfreh_listings(Tofino_daily_prop)
Tofino_freh_revenue <- freh_revenue(Tofino_daily_prop)
Tofino_vfreh_revenue <- vfreh_revenue(Tofino_daily_prop)

write.csv(Tofino_daily_prop$spread, "Tofino_daily_prop.csv")
write.csv(Tofino_rev_month$spread, "Tofino_rev_month.csv")
write.csv(Tofino_active_r$spread, "Tofino_active_r.csv")
write.csv(Tofino_eh_listings$spread, "Tofino_eh_listings.csv")
write.csv(Tofino_eh_rev$spread, "Tofino_eh_rev.csv")
write.csv(Tofino_freh_listings$spread, "Tofino_freh_listings.csv")
write.csv(Tofino_vfreh_listings$spread, "Tofino_vfreh_listings.csv")
write.csv(Tofino_freh_revenue$spread, "Tofino_freh_revenue.csv")
write.csv(Tofino_vfreh_revenue$spread, "Tofino_vfreh_revenue.csv")


####Everything below here is per 12-month period


#Active R > 0 Tables BY TM
active_listings_TM <- function(daily_prop){
  active_listings_TM_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R",], TMcode,City, Property_I), n()) #summarize daily_prop by TMcode, city, and Property_ID to get count of Rs per PID
  active_listings_TM_tidy <- aggregate(active_listings_TM_tidy_PID$`n()`, list(active_listings_TM_tidy_PID$TMcode, active_listings_TM_tidy_PID$City), length) #tidy form of R counts for R>0 per TMcode per city
  names(active_listings_TM_tidy) <- c("TMcode", "City", "R_gt_0") #rename columns
  active_listings_TM_spread <- spread(data = active_listings_TM_tidy, #spread (Excel) form of R counts (R>0 for any given TMcode) per TMcode per city
                                      key = TMcode,
                                      value = 'R_gt_0',
                                      drop=FALSE)
  active_listings_TM  <- list("tidy" = active_listings_TM_tidy, "spread" = active_listings_TM_spread)
}


# Revenue Tables BY TM
active_revenue_TM <- function(daily_prop){
  active_revenue_TM_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R",], TMcode, City), sum(Price)) #tidy form of revenue table for active listings per TMcode per city
  active_revenue_TM_spread <- spread(data = active_revenue_TM_tidy,  #spread (Excel) form of revenue table for active listings per TMcode per city
                                     key = TMcode,
                                     value = `sum(Price)`,
                                     drop=FALSE)
  active_revenue_TM  <- list("tidy" = active_revenue_TM_tidy, "spread" = active_revenue_TM_spread)
}


#Active (R>0 in a given month) EH Listings Tables BY TM
eh_listings_TM <- function(daily_prop){
  r_eh_tm_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #summarize daily_prop by TMcode, city, and Property_ID to get count of EH Rs per PID
  eh_listings_tm_tidy <- aggregate(r_eh_tm_tidy_PID$`n()`, list(r_eh_tm_tidy_PID$TMcode, r_eh_tm_tidy_PID$City), length) #tidy form of EH R counts for R>0 per TMcode per city
  names(eh_listings_tm_tidy) <- c("TMcode", "City", "R_gt_0") #rename columns
  eh_listings_tm_spread <- spread(data = eh_listings_tm_tidy, #spread (Excel) form of EH R counts (R>0 for any given 12-month period) per TMcode per city
                                  key = TMcode,
                                  value = 'R_gt_0',
                                  drop=FALSE)
  eh_listings_TM  <- list("tidy" = eh_listings_tm_tidy, "spread" = eh_listings_tm_spread)
}


# Active (R>0 in a given month) EH Revenue Tables BY TM
eh_revenue_TM <- function(daily_prop){
  eh_revenue_tm_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode, City), sum(Price)) #tidy form of EH revenue table for active listings per TMcode per city
  eh_revenue_tm_spread <- spread(data = eh_revenue_tm_tidy,  #spread (Excel) form of EH revenue table for active listings per TMcode per city
                                 key = TMcode,
                                 value = `sum(Price)`,
                                 drop=FALSE)
  eh_rev_TM  <- list("tidy" = eh_revenue_tm_tidy, "spread" = eh_revenue_tm_spread)
}

#run through all TM functions for Tofino NEED TO BE CHANGED TO NEW FUNCTION NAMES
Tofino_active_r_TM <- active_r_TM(Tofino_daily_prop)
Tofino_active_rev_TM <- active_revenue_TM(Tofino_daily_prop)
Tofino_eh_listings_TM <- eh_listings_TM(Tofino_daily_prop)
Tofino_eh_rev_TM <- eh_revenue_TM(Tofino_daily_prop)


write.csv(Tofino_active_r_TM$spread, "Tofino_active_r_TM.csv")
write.csv(Tofino_active_rev_TM$spread, "Tofino_active_rev_TM.csv")
write.csv(Tofino_eh_listings_TM$spread, "Tofino_eh_listings_TM.csv")
write.csv(Tofino_eh_rev_TM$spread, "Tofino_eh_rev_TM.csv")