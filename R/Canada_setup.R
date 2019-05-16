## INITIAL FILE IMPORT AND CLEANING

library(tidyverse)
library(rgdal)
library(sf)
library(maptools)
library(tmap)
library(ggmap)
library(parallel)
library(data.table)


Canada_daily <- read.csv("data/Canada_daily.csv",stringsAsFactors=FALSE)
Canada_daily <- Canada_daily[,c(1:5,8)]
Canada_daily$Date <- as.Date(Canada_daily$Date,"%Y-%m-%d")
names(Canada_daily) <- c("Property_I","Date","Status","Booked","Price","Reservation")

Canada_property <- read.csv("data/Canada_property.csv",stringsAsFactors=FALSE)
Canada_property$Housing <- NULL
names(Canada_property) <- c("Property_I","Host_ID","Property_T","Listing_Ty","Created","Scraped","DAUID","CTUID","ADAUID","City","City_type","Province")
Canada_property[Canada_property$City=="Montr\xe9al",]$City <- c("Montreal")
Canada_property[Canada_property$City=="Qu\xe9bec",]$City <- c("Quebec")
Canada_property[Canada_property$City=="Rivi\xe8re-du-Loup",]$City <- c("Riviere-du-Loup")
Canada_property[Canada_property$City=="Sept-\xceles",]$City <- c("Sept-Iles")
Canada_property[Canada_property$City=="Trois-Rivi\xe8res",]$City <- c("Trois-Rivieres")
Canada_property[Canada_property$City=="Campbellton (New Brunswick part / partie du Nouveau-Brunswick)",]$City <- c("Cambellton")
Canada_property[Canada_property$Province=="British Columbia / Colombie-Britannique",]$Province <- c("British Columbia")
Canada_property[Canada_property$Province=="Quebec / Qu\xe9bec",]$Province <- c("Quebec")
Canada_property[Canada_property$Province=="Nova Scotia / Nouvelle-\xc9cosse",]$Province <- c("Nova Scotia")
Canada_property[Canada_property$Province=="Newfoundland and Labrador / Terre-Neuve-et-Labrador",]$Province <- c("Newfoundland")
Canada_property[Canada_property$Province=="New Brunswick / Nouveau-Brunswick",]$Province <- c("New Brunswick")
Canada_property[Canada_property$Province=="Prince Edward Island / \xcele-du-Prince-\xc9douard",]$Province <- c("Prince Edward Island")
Canada_property[Canada_property$Province=="Northwest Territories / Territoires du Nord-Ouest",]$Province <- c("Northwest Territories")
Canada_property$Created <- as.Date(Canada_property$Created, "%m/%d/%Y")
Canada_property$Scraped <- as.Date(Canada_property$Scraped, "%m/%d/%Y")
Canada_property <- Canada_property[order(Canada_property$Property_I),]
Canada_property <- Canada_property[Canada_property$Property_I %in% Canada_daily$Property_I,]

length(unique(Canada_daily$Property_I))
length(unique(Canada_property$Property_I))

Canada_daily <- Canada_daily[Canada_daily$Property_I %in% Canada_property$Property_I,]

reducer <- function(property,daily, cores=4){
  library(parallel)
  library(data.table)
  PID_list <- split(daily,daily$Property_I)
  prop_list <- split(property,property$Property_I)
  compare <- function(daily2,property2) {
    daily2[daily2$Date >= property2$Created & daily2$Date <= property2$Scraped,]
  }
  output <- mcmapply(compare,PID_list,prop_list,SIMPLIFY=FALSE,mc.cores=cores)
  rbindlist(output)
}

Canada_daily <- reducer(Canada_property,Canada_daily, cores=10)

system.time(reducer(Canada_property,Canada_daily, cores=4))
system.time(reducer(Canada_property,Canada_daily, cores=5))
system.time(reducer(Canada_property,Canada_daily, cores=6))
system.time(reducer(Canada_property,Canada_daily, cores=7))
system.time(reducer(Canada_property,Canada_daily, cores=8))
system.time(reducer(Canada_property,Canada_daily, cores=9))
system.time(reducer(Canada_property,Canada_daily, cores=10))

