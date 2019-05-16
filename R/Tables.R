##do I have to worry about housing or was that already removed?

#select points within Tofino's DAUIDs and subset them from property file
Tofino_prop <- subset(Canada_property, Canada_property$DAUID == "59230087" | Canada_property$DAUID == "59230088"| Canada_property$DAUID == "59230089"| Canada_property$DAUID == "59230090")
#subset them from daily file
Tofino_daily <- Canada_daily[Canada_daily$Property_I %in% Tofino_prop$Property_I,]

#Setup
setup <- function(property, daily) {

library(tidyr)
library(zoo)
library(dplyr)
daily_prop <- merge(daily, property, by = c("Property_I")) #merge daily and property files
daily_prop$monthYear <- as.Date(as.yearmon(daily_prop$Date)) #create monthYear column
daily_prop$Year <- format(as.Date(daily_prop$Date, format="%d/%m/%Y"),"%Y") #create Year column
daily_prop$TMcode <- "N/A" #create new field to code LTM, MTM, FTM, etc.
daily_prop[daily_prop$Date < "2015-05-01",]$TMcode <- "1"
daily_prop[daily_prop$Date < "2016-05-01" & daily_prop$Date > "2015-04-30",]$TMcode <- "2"
daily_prop[daily_prop$Date < "2017-05-01" & daily_prop$Date > "2016-04-30",]$TMcode <- "3"
daily_prop[daily_prop$Date < "2018-05-01" & daily_prop$Date > "2017-04-30",]$TMcode <- "4"
daily_prop
}

daily_prop <- setup(Tofino_prop, Tofino_daily)




# Revenue Tables

revenue_month <- function(daily_prop) {
  Tofino_rev_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R",], monthYear, City), sum(Price)) #tidy form of revenue table for active listings per month per city
  Tofino_rev_spread <- spread(data = Tofino_rev_tidy,  #spread (Excel) form of revenue table for active listings per month per city
               key = monthYear,
               value = `sum(Price)`,
               drop=FALSE)
  rev_month <- list("tidy" = Tofino_rev_tidy, "spread" = Tofino_rev_spread)
  }

Tofino_rev_month <- revenue_month(Tofino_daily_prop)





#Active R > 0 Tables



r_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R",], monthYear,City, Property_I), n()) #summarize daily_prop by monthYear, city, and Property_ID to get count of Rs per PID
r_tidy <- aggregate(r_tidy_PID$`n()`, list(r_tidy_PID$monthYear, r_tidy_PID$City), length) #tidy form of R counts for R>0 per month per city
names(r_tidy) <- c("Date", "City", "R_gt_0") #rename columns
r_spread <- spread(data = r_tidy, #spread (Excel) form of R counts (R>0 for any given month) per month per city
                   key = Date,
                   value = 'R_gt_0',
                   drop=FALSE)

#Active A+R > 0 Tables
ar_tidy_PID <- summarise(group_by(daily_prop[!daily_prop$Status=="B",], monthYear,City, Property_I), n()) #summarize daily_prop by monthYear, city, and Property_ID to get count of A+R per PID
ar_tidy <- aggregate(ar_tidy_PID$`n()`, list(ar_tidy_PID$monthYear, ar_tidy_PID$City), length) #tidy form of A+R counts for R>0 per month per city
names(ar_tidy) <- c("Date", "City", "R_gt_0") #rename columns
ar_spread <- spread(data = ar_tidy, #spread (Excel) form of A+R counts per month per city
                   key = Date,
                   value = 'R_gt_0',
                   drop=FALSE)

#Active (R>0 in a given month) EH Listings Tables
r_eh_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], monthYear,City, Property_I), n()) #summarize daily_prop by monthYear, city, and Property_ID to get count of EH Rs per PID
r_eh_tidy <- aggregate(r_eh_tidy_PID$`n()`, list(r_eh_tidy_PID$monthYear, r_eh_tidy_PID$City), length) #tidy form of EH R counts for R>0 per month per city
names(r_eh_tidy) <- c("Date", "City", "R_gt_0") #rename columns
r_eh_spread <- spread(data = r_eh_tidy, #spread (Excel) form of EH R counts (R>0 for any given month) per month per city
                      key = Date,
                      value = 'R_gt_0',
                      drop=FALSE)

# Active (R>0 in a given month) EH Revenue Tables 
rev_eh_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], monthYear, City), sum(Price)) #tidy form of EH revenue table for active listings per month per city
rev_eh_spread <- spread(data = rev_eh_tidy,  #spread (Excel) form of EH revenue table for active listings per month per city
                        key = monthYear,
                        value = `sum(Price)`,
                        drop=FALSE)

#FREH counts
FREH_R_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #get number of R EH per PID per TMcode
FREH_AR_PID <- summarise(group_by(daily_prop[!daily_prop$Status=="B"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #get number of A+R EH per PID per TMcode

FREH_A_AR <- merge(FREH_R_PID, FREH_AR_PID, by = c("Property_I", "TMcode")) #merge R and AR counts into one file
names(FREH_A_AR) <- c("Property_I", "TMcode", "City", "R","City", "A_R") #rename columns

FREH_tidy <- aggregate(FREH_A_AR[FREH_A_AR$R>59 &FREH_A_AR$A_R>119,]$TMcode, list(FREH_A_AR[FREH_A_AR$R>59 &FREH_A_AR$A_R>119,]$TMcode, FREH_A_AR[FREH_A_AR$R>59 &FREH_A_AR$A_R>119,]$City), length) #tidy count of FREH listings per month per city
names(FREH_tidy) <- c("TMcode", "City", "FREH") #rename columns
FREH_spread <- spread(data = FREH_tidy, #spread (Excel) form of FREH counts per month per city
                   key = TMcode,
                   value = 'FREH',
                   drop=FALSE)

#VFREH counts
VFREH_R_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #get number of R EH per PID per year (same exact output as first step in FREH counts)
VFREH_AR_PID <- summarise(group_by(daily_prop[!daily_prop$Status=="B"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n())#get number of A+R EH per PID pe year (same exact output as second step in FREH counts)

VFREH_A_AR <- merge(VFREH_R_PID, VFREH_AR_PID, by = c("Property_I", "TMcode")) #merge R and AR counts into one file (same exact output as third step in FREH counts)
names(VFREH_A_AR) <- c("Property_I", "TMcode", "City", "R","City", "A_R") #rename columns (same exact output as fourth step in FREH counts)

VFREH_tidy <- aggregate(VFREH_A_AR[VFREH_A_AR$R>119 &VFREH_A_AR$A_R>239,]$R, list(VFREH_A_AR[VFREH_A_AR$R>119 &VFREH_A_AR$A_R>239,]$TMcode, VFREH_A_AR[VFREH_A_AR$R>119 &VFREH_A_AR$A_R>239,]$City), length) #tidy count of VFREH listings per month per city

names(VFREH_tidy) <- c("TMcode", "City", "FREH") #rename columns
VFREH_spread <- spread(data = VFREH_tidy, #spread (Excel) form of VFREH counts per month per city
                      key = TMcode,
                      value = 'FREH',
                      drop=FALSE)


#FREH Revenue
FREH_Rev_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), sum(Price)) #get sum of price for all reservations per property per TMcode
freh_rev2 <- merge(FREH_Rev_PID, FREH_A_AR, by = c("Property_I", "TMcode")) #merge sum of price with R and AR counts from previous step
names(freh_rev2) <- c("Property_I", "TMcode", "City", "Price", "City","R","City", "A_R") #rename columns
FREH_rev_tidy <- aggregate(freh_rev2[freh_rev2$R>59 &freh_rev2$A_R>119,]$Price, list(freh_rev2[freh_rev2$R>59 &freh_rev2$A_R>119,]$TMcode, freh_rev2[freh_rev2$R>59 &freh_rev2$A_R>119,]$City), sum) #tidy sum of revenue of FREH listings per month per city
names(FREH_rev_tidy) <- c("TMcode","City", "Price")
FREH_rev_spread <- spread(data = FREH_rev_tidy, #spread (Excel) form of revenue of FREH per month per city
                       key = TMcode,
                       value = 'Price',
                       drop=FALSE)

#VFREH Revenue
VFREH_Rev_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), sum(Price)) #get sum of price for all reservations per property per TMcode (same as FREH)
vfreh_rev2 <- merge(VFREH_Rev_PID, FREH_A_AR, by = c("Property_I", "TMcode")) #merge sum of price with R and AR counts from previous step (same as FREH)
names(vfreh_rev2) <- c("Property_I", "TMcode", "City", "Price", "City","R","City", "A_R") #rename columns
VFREH_rev_tidy <- aggregate(vfreh_rev2[vfreh_rev2$R>119 &vfreh_rev2$A_R>239,]$Price, list(vfreh_rev2[vfreh_rev2$R>119 &vfreh_rev2$A_R>239,]$TMcode, vfreh_rev2[vfreh_rev2$R>119 &vfreh_rev2$A_R>239,]$City), sum) #tidy sum of revenue of VFREH listings per month per city
names(VFREH_rev_tidy) <- c("TMcode","City", "Price") #rename columns
VFREH_rev_spread <- spread(data = VFREH_rev_tidy, #spread (Excel) form of revenue of VFREH listings per month per city
                          key = TMcode,
                          value = 'Price',
                          drop=FALSE)




####Everything below here is per 12-month period


#Active R > 0 Tables BY TM
r_TM_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R",], TMcode,City, Property_I), n()) #summarize daily_prop by TMcode, city, and Property_ID to get count of Rs per PID
r_TM_tidy <- aggregate(r_TM_tidy_PID$`n()`, list(r_TM_tidy_PID$TMcode, r_TM_tidy_PID$City), length) #tidy form of R counts for R>0 per TMcode per city
names(r_TM_tidy) <- c("TMcode", "City", "R_gt_0") #rename columns
r_TM_spread <- spread(data = r_TM_tidy, #spread (Excel) form of R counts (R>0 for any given TMcode) per TMcode per city
                   key = TMcode,
                   value = 'R_gt_0',
                   drop=FALSE)


# Revenue Tables BY TM
rev_TM_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R",], TMcode, City), sum(Price)) #tidy form of revenue table for active listings per TMcode per city
rev_TM_spread <- spread(data = rev_TM_tidy,  #spread (Excel) form of revenue table for active listings per TMcode per city
                     key = TMcode,
                     value = `sum(Price)`,
                     drop=FALSE)

#Active (R>0 in a given month) EH Listings Tables BY TM
r_eh_tm_tidy_PID <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode,City, Property_I), n()) #summarize daily_prop by TMcode, city, and Property_ID to get count of EH Rs per PID
r_eh_tm_tidy <- aggregate(r_eh_tm_tidy_PID$`n()`, list(r_eh_tm_tidy_PID$TMcode, r_eh_tm_tidy_PID$City), length) #tidy form of EH R counts for R>0 per TMcode per city
names(r_eh_tm_tidy) <- c("TMcode", "City", "R_gt_0") #rename columns
r_eh_tm_spread <- spread(data = r_eh_tm_tidy, #spread (Excel) form of EH R counts (R>0 for any given 12-month period) per TMcode per city
                      key = TMcode,
                      value = 'R_gt_0',
                      drop=FALSE)

# Active (R>0 in a given month) EH Revenue Tables BY TM
rev_eh_tm_tidy <- summarise(group_by(daily_prop[daily_prop$Status=="R"&daily_prop$Listing_Ty=="Entire home/apt",], TMcode, City), sum(Price)) #tidy form of EH revenue table for active listings per TMcode per city
rev_eh_tm_spread <- spread(data = rev_eh_tm_tidy,  #spread (Excel) form of EH revenue table for active listings per TMcode per city
                        key = TMcode,
                        value = `sum(Price)`,
                        drop=FALSE)




