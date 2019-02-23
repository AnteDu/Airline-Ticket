#clear workspace
rm(list=ls())

#load in packages
library("tidyverse")
library("data.table") 
library("ggplot2")
library("knitr")
library("rmarkdown")

options(scipen=999)
#Read in DB1B data
setwd("~/Econ_690")

data <- read.csv("/Users/ante/Desktop/Econ690PS1/690PS1/market.csv", sep = ",", header = TRUE)
str(data)

#lowercase column names
colnames(data) = tolower(colnames(data))

#remove itineraries with a surface segment and those with multiple carriers
data = data[as.logical(abs(1-data$tk_carrier_change)),]

#Omit any fare less than or equal to 25 and bigger than or equal to 2500
data = data[(data$market_fare>25&data$market_fare<2500), ]

#Omit small markets
total_passengers_mt = as.data.frame(data.table(data)[,sum(passengers),by=c("origin_airport_id","dest_airport_id")])
data = merge(data,total_passengers_mt,by=c("origin_airport_id","dest_airport_id"))
data = data[-which(data$V1<182.5),] #omit markets with fewer than 20 passengers per day

data$V1 = NULL
################################################################################################################
#market-airline data

datama = as.data.frame(data.table(data)[,.(
  price = weighted.mean(market_fare,passengers,na.rm=T),
  total_passengers = sum(passengers,na.rm=T),
  distance = weighted.mean(market_distance,passengers,na.rm=T)
  ),by=c("origin_airport_id", "dest_airport_id","ticket_carrier")
  ])
################################################################################################################
#get market level data using tidyverse
datam = datama %>%
  group_by(origin_airport_id, dest_airport_id) %>%
  nest()

#get market level characteristics 
datam$num_carriers = unlist(lapply(datam[["data"]],function(x) {
  return(nrow(x))
}))

datam$average_price_m = unlist(lapply(datam[["data"]],function(x) {
  return(weighted.mean(x$price,x$total_passengers))
}))

datam$hhi = unlist(lapply(datam[["data"]],function(x) {
  within_shares = (x$total_passengers/sum(x$total_passengers))*100
  return(sum(within_shares^2))
}))

datam$average_distance_m = unlist(lapply(datam[["data"]],function(x) {
  return(weighted.mean(x$distance,x$total_passengers))
}))

#remove list from datam
datam = datam[,c("origin_airport_id","dest_airport_id","num_carriers","average_price_m",
                 "hhi","average_distance_m")]

#load in population data and merge it with market-level data

load("/Users/ante/Desktop/Econ690PS1/690PS1/populations.R")
datae = merge(populations,mydataML)
datae <-
  datae %>%
  group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID) %>%
  mutate(size = (population_origin*population_dest)^(1/2))

#print tables
datama %>% select(c("price","total_passengers","distance")) %>% summary() %>% kable()
datam %>% select(c("average_price_m","average_distance_m","num_carriers","hhi","market_size")) %>% kable()

#plots
ggplot(datam,aes(x=hhi,y=average_price_m)) +
  ggtitle("Price and Market Structure") + 
  xlab("HHI") + ylab("Price") +
  geom_point()

ggplot(datam,aes(x=hhi)) + 
  ggtitle("HHI Density") + 
  xlab("HHI") + ylab("Density") +
  geom_density()

ggplot(datam,aes(x=average_price_m)) + 
  ggtitle("Price Density") + 
  xlab("Price") + ylab("Density") +
  geom_density()

#save market-level data for PS-2
save(datam,file="airline_data_market_level.R")


