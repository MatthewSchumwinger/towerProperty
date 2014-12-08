## MS script to process account$billing.geo.code
# add "hotspots"

# us no us geos
# standardize geos

# group geos by city, state
# map geos

library(stringr)
library(fields)
library(mi)

geo <- rawData$accounts

geo <- as.data.frame(geo[, c(1,3,5)]) #c(1,3)
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

## inspect
geo$billing.zip.code[geo$billing.zip.code ==""] <- NA
geo$billing.city[geo$billing.city ==""] <- NA
mp.plot(geo, y.order = TRUE, x.order = F, clustered = FALSE, gray.scale = TRUE)

## tag 1 if originally NULL in billing.zip.code and billing.city; 0 otherwise
geo$missing <- 0
geo$missing[is.na(geo$billing.zip.code)&is.na(geo$billing.city)] <- 1
table(geo$missing)

# read in and process zip code directory
zipDir <- read.csv('data/free-zipcode-database-Primary.csv',colClasses='character')
# add missing zero to four-digit US geos
for (i in 1:dim(zipDir)[[1]]){
  if(str_length(zipDir[i, 1]) < 5){
    zipDir[i,1] <- str_pad(zipDir[i,1], 5, "left", "0")
  }
}
table(str_length(zipDir[,1]))
zipDir <- subset(zipDir, select = c(Zipcode,City,State,Lat,Long)) # keep only relevent fields

# merge city, state info to geo
geo <- merge(geo, zipDir, by.x="billing.zip.code", by.y="Zipcode",all.x=T)
names(geo)
# merge in hotspots from QGIS
hot <- read.csv("data/hotspot.csv", as.is=T)
hot <- hot[,c(4,16)]
geo <- merge(geo, hot, by="account.id", all.x=T)


# tag accounts with null billing.zip.codes as "CA" if billing.city is a California city
geo[19751,3] <- NA #removed value with troublesome "/"
caCity <- as.data.frame(table(subset(geo, State=="CA", select=City))) #list of CA cities
noZip <- is.na(geo$billing.zip.code) # index of accounts with no billing.zip.code value
df.noZip <- subset(geo, is.na(billing.zip.code)) 

######## this is code I can't get to work ##################
#geo$test <- "" # create temp column to test code. If it works, then update directly to geo$State
#for (i in 1:973){
#  geo$test[str_detect(as.character(geo[noZip,3]), ignore.case(as.character(caCity[i,1])))] <- "CA" # need to subset for missing
#}
#table(geo$test) # CA must be less than 2955
###########################################################
noZipIndices = which(noZip)
for(j in noZipIndices){
  for (i in 1:973){
    city = str_trim(tolower(as.character(geo[j,3])))
    cal = str_trim(tolower(as.character(caCity[i,1])))
    if(!is.na(city) && !is.na(cal) && city == cal) {
      print(paste("Assigned ", j, " city ", city))
      geo$State[j] <- "CA" # need to subset for missing
      break
    }
  }
}
table(geo$State) 

#######

# dump csv with geos for use as categorical predictors
write.csv(geo, "data/geo.account.csv", row.names=F)

# add distance from account to the 3 locations
geo <- read.csv("data/geo.account.csv")

# locations
dBerkley <- c(37.867005,-122.261542)
dSF <- c(37.7763272,-122.421545)
dPeninsula <- c(37.4320436,-122.1661352)
venues <- rbind(dBerkley, dSF, dPeninsula)
venues <- venues[,c(2,1)]
colnames(venues) = c("Long","Lat")
locDist <- rdist.earth(geo[,c(8,7)], venues)
geo <- cbind(geo,locDist)


## dump csv for use in data.r
write.csv(geo, "data/geo.account.csv", row.names=F)

######## top predictors for mapping (uses main.gbm.r objects) ######
topPred = summary(gbm.orch)
write.csv(topPred, "topPred.csv", row.names=F)
# dump csv for mapping
geo <- merge(geo, data$allSetAll, by="account.id", all.y=T)
colnames(geo)[1] <- "accountID"
colnames(geo)[2] <- "billingZipCode"
write.csv(geo, "viz/topPred.csv", row.names=F)
# dump locations for mapping
venues <- venues[,c(2,1)]
write.csv(venues, "viz/venues.csv", row.names=T)
#need to add field name to row names manually




####################################
#######  old code below ############

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







