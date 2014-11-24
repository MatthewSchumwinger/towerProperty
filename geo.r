## MS script to process account$billing.geo.code
# add "hotspots"

# us no us geos
# standardize geos

# group geos by city, state
# map geos

library(stringr)

geo <- rawData$accounts
geo <- as.data.frame(geo[, c(1,3)])
table(str_length(geo[,2]))

# add missing zero to four-digit US geos
for (i in 1:19833){
  if(str_length(geo[i, 2]) == 4){
    geo[i,2] <- str_pad(geo[i,2], 5, "left", "0")
    print(geo[i,2])
  }
}
table(str_length(geo[,2]))


# trim +4 from nine-digit US geos
for (i in 1:19833){
  if(str_length(geo[i, 2]) == 10){
    geo[i,2] <- str_split_fixed(geo[i,2], "-", 2)[1]
    print(geo[i,2])
  }
}
table(str_length(geo[,2]))


# read in and process zip code directory
zipDir <- read.csv('data/free-zipcode-database-Primary.csv',colClasses='character')
# add missing zero to four-digit US geos
for (i in 1:dim(zipDir)[[1]]){
  if(str_length(zipDir[i, 1]) < 5){
    zipDir[i,1] <- str_pad(zipDir[i,1], 5, "left", "0")
  }
}
table(str_length(zipDir[,1]))

# merge city, state info to geo
geo <- merge(geo, zipDir, by.x="billing.zip.code", by.y="Zipcode",all.x=T)
names(geo)
# merge in hotspots from QGIS
hot <- read.csv("data/hotspot.csv", as.is=T)
hot <- hot[,c(4,16)]
geo <- merge(geo, hot, by="account.id", all.x=T)



# dump csv with geos for use as categorical predictors
write.csv(geo, "data/geo.account.csv", row.names=F)


####### ------ old code below ############

# add $is.us for US/non-us accounts ## inserted into data.r
rawData$accounts$is.us = 1 # MS: tag foreign accounts by geo
for (i in 1:dim(rawData$accounts)[1]){
  if(str_detect(rawData$accounts[i, 3], "[A-Z]|[a-z]")){
    rawData$accounts$is.us[i] <- 0
    print(rawData$accounts[i, c(1,3,11)])
  }
}


# dump csv with geos for geocoding
geo.list <- as.data.frame(table(geo[,2]))
names(geo.list) <- c("geo", "count")
write.csv(geo.list, "data/billing.geo.csv", row.names=F)



geo = read.csv('data/geo.account.csv',colClasses='character')
rawData$geo <- geo
data <- rawData

catGeo <- c("State", "City") # categorical variables
numGeo <- c("Lat", "Long") # numeric variables
data$geoFactors = data$geo[, c("account.id", catGeo)]
data$geoFactors[catGeo] = sapply(data$geoFactors[catGeo], as.factor) 
data$geoNum = data$geo[, c("account.id", numGeo)]


data$accounts$geo.state = "" # MS: add state predictor
states = data$geo[, c("account.id", "State")]
data$accounts$geo.state = merge(data$accounts, states, by="account.id", all.x=T) # MS: pull in state from zip code merge



numGeo <- c("Lat", "Lon")







