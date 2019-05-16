
revenue_field <- function(daily,property,start="2017-09-01",end="2018-08-31",cores=6) {
  
  # Set up daily list
  daily <- daily[daily$Date >= as.Date(start,origin="1970-01-01") & daily$Date <= as.Date(end,origin="1970-01-01"),]
  daily_list <- split(daily,daily$Property_I)
  
  # Set up property list
  property <- property[order(property$Property_I),]
  property <- property[property$Property_I %in% daily$Property_I,]
  property_list <- split(property,property$Property_I)
  
  # Revenue aggregator
  revenue <- function(daily,property){
    sum(daily[daily$Status=="R",]$Price)
  }
  
  # Add field and pass output 
  property$Revenue <- mcmapply(revenue,daily_list,property_list,mc.cores=cores)
  property
  
}

host_percentiles <- function(property) {
  
  host_list <- split(property,property$Host_ID)
  host_rev <- sapply(host_list,function(x) sum(x$Revenue))
  host_rev <- host_rev[host_rev>0]
  host_rev <- sort(host_rev,decreasing=TRUE)
  
  h99 <- sum(host_rev[1:as.integer(0.01*length(host_rev))])/sum(host_rev)
  h90 <- sum(host_rev[1:as.integer(0.1*length(host_rev))])/sum(host_rev)
  h80 <- sum(host_rev[1:as.integer(0.2*length(host_rev))])/sum(host_rev)
  
  c(h80,h90,h99)
  
}


revenue_table <- function(listings,daily,start="2014-09-01",end="2018-04-30",cores=6){
  
  library("dplyr")
  library("parallel")
  library("zoo")
  
  Date <- seq(as.Date(start, origin="1970-01-01"),as.Date(end,origin="1970-01-01"),"day") # Create list of dates
  date_range <- as.list(as.Date(start, origin="1970-01-01"):as.Date(end, origin="1970-01-01")) # Divide listings by dates
  
  Revenue <- unlist(mclapply(date_range,function(x) sum(daily[daily$Date==x&daily$Status=="R",5]),mc.cores=cores))
  Revenue_cumulative <- rollsum(Revenue,365,fill=NA,align="right")
  data_frame(Date,Revenue,Revenue_cumulative)
}

