# us no us zips
# standardize zips

# group zips by city, state
# map zips

library(stringr)

zip <- rawData$accounts
zip <- as.data.frame(zip[, c(1,3)])
table(str_length(zip[,2]))

# add missing zero to four-digit US zips
for (i in 1:19833){
  if(str_length(zip[i, 2]) == 4){
    zip[i,2] <- str_pad(zip[i,2], 5, "left", "0")
    print(zip[i,2])
  }
}
table(str_length(zip[,2]))


# trim +4 from nine-digit US zips
for (i in 1:19833){
  if(str_length(zip[i, 2]) == 10){
    zip[i,2] <- str_split_fixed(zip[i,2], "-", 2)[1]
    print(zip[i,2])
  }
}
table(str_length(zip[,2]))


# add billing zip code bin variable
# bin foreign accounts
zip$billing.zip.bin <- ""
for (i in 1:19833){
  if(str_detect(zip[i, 2], "[A-Z]|[a-z]")){
    zip$billing.zip.bin[i] <- "foreign"
    print(zip[i, c(2,3)])
  }
}
table(zip[,3])





rawData$accounts$is.us = 1 # MS: tag foreign accounts by zip
for (i in 1:dim(rawData$accounts)[1]){
  if(str_detect(rawData$accounts[i, 3], "[A-Z]|[a-z]")){
    rawData$accounts$is.us[i] <- 0
    print(rawData$accounts[i, c(1,3,11)])
  }
}

table(str_detect(rawData$accounts[, 3], "[A-Z]|[a-z]"))
dim(rawData$accounts)
table(rawData$accounts[,11])
