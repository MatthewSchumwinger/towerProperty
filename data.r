
readData = function(useLogTransform) {
  
  train = read.csv('data\\train.csv')
  
  if(useLogTransform) {
    train$total = log(1+train$total)  
  }
  
  subscriptions = read.csv('data\\subscriptions.csv',colClasses='character') # MS: include all variables
  #subscriptions = subscriptions[,c("account.id", "price.level", "no.seats", "total", "season" )]

  accounts = read.csv('data\\account.csv',colClasses='character') #Kartik added
  #accounts = accounts[,c("account.id", "amount.donated.2013", "amount.donated.lifetime","no.donations.lifetime")] #Kartik added
  # next step "billing.zip.code"
  
  tickets = read.csv('data\\tickets.csv',colClasses='character')
  tickets = tickets[,c("account.id",  "season", "set", "price.level", "no.seats")]
  # should we do that or drop???
  tickets$no.seats[nchar(tickets$no.seats) == 0] = 1
  tickets$set[nchar(tickets$set) == 0] = 0
  tickets$price.level[nchar(tickets$price.level) == 0] = 1
  tickets$price.level[nchar(tickets$price.level) > 1] = 1
  
  concert = read.csv('data\\concert_table_summary.csv',colClasses='character')

  concert_table = read.csv('data\\concert_table_set_2014.csv',colClasses='character')
  concert_table = concert_table[, ! (colnames(concert_table) %in% c("concert.name", "who", "what"))]
  
  
  return (list("train"=train, "subscriptions"=subscriptions, "accounts"=accounts, "concert"=concert, "tickets"=tickets, "concert_table"=concert_table))
}

preparePredictors = function(data, filterRegex, validationRatio) {
  
  numVar = c("amount.donated.2013", "amount.donated.lifetime", "no.donations.lifetime", "years.donating", "is.us")
  catVar = c("billing.city", "relationship")

  data$accounts$first.donated = substr(data$accounts$first.donated, 1, 4)
  data$accounts$first.donated[data$accounts$first.donated == ""] = "2014"
  data$accounts$first.donated = sapply(data$accounts$first.donated, as.numeric)
  data$accounts$years.donating = rep(2014, length(data$accounts$first.donated))
  data$accounts$years.donating = data$accounts$years.donating - data$accounts$first.donated
  data$accounts$is.us = sapply(!(data$accounts$billing.zip.code == ""), as.numeric) 
  
  for(cat in catVar) {
    data$accounts[,cat] = normalizeStrings(data$accounts[,cat])
  }
    
  data$accounts[numVar] = sapply(data$accounts[numVar], as.numeric) 
  data$accounts[catVar] = sapply(data$accounts[catVar], as.factor) 
  
  data$accountsFactor = data$accounts[c("account.id", catVar)]
  data$accounts = data$accounts[c("account.id", numVar)]
  
  
  #additional data based on accounts
  data$accounts$add_donated.2013 = sapply((data$accounts$amount.donated.2013 > 0), as.numeric)
  data$accounts$add_no.donations.lifetime.if.donated.2013 = data$accounts$add_donated.2013 * data$accounts$no.donations.lifetime

  #avg donation
  data$accounts$add_avg.donation = ifelse(data$accounts$no.donations.lifetime==0, 0, data$accounts$amount.donated.lifetime / data$accounts$no.donations.lifetime)
  #avg donation if donated last year
  data$accounts$add_avg.donation.if.donated.2013 = data$accounts$add_donated.2013 * data$accounts$add_avg.donation
  
  ### -- new code from MS to bring in categorical variables -- ## 
  ### -- and properly classify as numeric, or as factors    -- ##
  numVar <- c("price.level", "no.seats", "total") # numeric variables
  catVar <- c("package", "location", "section", "multiple.subs") # categorical variables
  
  for(cat in catVar) {
    data$subscriptions[,cat] = normalizeStrings(data$subscriptions[,cat])
  }
  
  # only convert NAs to zero for numeric variables
  data$subscriptionsFactors = data$subscriptions[, c("account.id", catVar, "season")]
  data$subscriptionsFactors[catVar] = sapply(data$subscriptionsFactors[catVar], as.factor) 
  
  data$subscriptions = data$subscriptions[,c("account.id", numVar, "season" )]
  data$subscriptions[,numVar] <- apply(data$subscriptions[, numVar], 2, function(x){replace(x, is.na(x), 0)}) #3534 NAs in $price.level changed to 0
  data$subscriptions[numVar] = sapply(data$subscriptions[numVar], as.numeric)
  
  
  data$concert[2:34] = sapply(data$concert[2:34], as.numeric) 
  concertLong = melt(data$concert, id='season')

  data$tickets[3:5] = sapply(data$tickets[3:5], as.numeric) 

  
  ticketsLong = melt(data$tickets, id=c("account.id", "season", "set"))
  ticketsLong$prefix = rep("add", nrow(ticketsLong))
    
  ticketsWide = dcast(ticketsLong, account.id~prefix+variable+season+set, value.var="value")
  
  ticketsPerSeason = dcast(data$tickets[1:3], account.id~season, value.var = "set", fun.aggregate = length)
  colnames(ticketsPerSeason)[2:5] = c("add_tickets_total_2010_2011", "add_tickets_total_2011_2012", "add_tickets_total_2012_2013", "add_tickets_total_2013_2014") 
  
  seatsPerSeason = dcast(data$tickets[c(1,2,5)], account.id~season, value.var = "no.seats", fun.aggregate = sum)
  colnames(seatsPerSeason)[2:5] = c("add_tickets_seats_2010_2011", "add_tickets_seats_2011_2012", "add_tickets_seats_2012_2013", "add_tickets_seats_2013_2014") 

  #########################
  # parsing concerts table#
  #########################
  data$concert_table[2:35] = sapply(data$concert_table[2:35], as.numeric) 
  concertsInSeasons = melt(data$concert_table, id = c("season", "set"))
  
  #2 possible versions weighted by the number of concerts that year or unweighted
  concertsInSeasonsWithFreq = dcast(concertsInSeasons, season~variable, value.var = "value", fun.aggregate = sum)
  #concertsInSeasonsWithoutFreq = dcast(concertsInSeasons, season~variable, value.var = "value", fun.aggregate = max)
  
  totalPerAccount = data$subscriptions[which(data$subscriptions$season != "2014-2015"),]
  totalPerAccount = totalPerAccount[,c("account.id", "total", "season")]
  invertedTotal = sapply(totalPerAccount$total == 0, as.numeric)
  
  #select either concertsInSeasonsWithFreq or concertsInSeasonsWithoutFreq for weighted on unweighted version
  totalPerAccountWithConcerts = merge(totalPerAccount, concertsInSeasonsWithFreq, by="season")
  
  #comment that line if you don't want to multiply by number of subscriptions bought that year
  totalPerAccountWithConcerts[4:36] = totalPerAccountWithConcerts[4:36] * totalPerAccountWithConcerts$total

  missedPerAccountWithConcerts = totalPerAccountWithConcerts
  missedPerAccountWithConcerts[4:36] = missedPerAccountWithConcerts[4:36] * invertedTotal
  
  # removing total variable as it is no longer needed
  totalPerAccountWithConcerts = totalPerAccountWithConcerts[,-3]
  missedPerAccountWithConcerts = missedPerAccountWithConcerts[,-3]
  
  accountsPreferences = melt(totalPerAccountWithConcerts, id = c("account.id", "season"))
  accountsPreferences = dcast(accountsPreferences, account.id~variable, value.var="value", fun.aggregate = sum)

  #adjusting for number of concerts in 2014 - should we filter only to those that will be played in 2014-2015?
  #accountsPreferencesAdjustedTo2014Concerts = accountsPreferences[,c("account.id", "BACH", "HANDEL", "TELEMAN", "JOHANN", "VIVALDI", "HAYDN", "ROSSINI")]
  #accountsPreferencesAdjustedTo2014Concerts = accountsPreferences
  accountsPreferencesAdjustedTo2014Concerts = accountsPreferences[,c("account.id", "BACH", "HANDEL", "TELEMAN", "JOHANN", "VIVALDI", "HAYDN", "ROSSINI", "CORELLI", "PERGOLESI", "MOZART")]
  
  colnum = ncol(accountsPreferencesAdjustedTo2014Concerts)
  colnames(accountsPreferencesAdjustedTo2014Concerts)[2:colnum] = paste("conc", colnames(accountsPreferencesAdjustedTo2014Concerts)[2:colnum], sep = "_")
  accountsPreferencesAdjustedTo2014Concerts[2:colnum] = sapply(accountsPreferencesAdjustedTo2014Concerts[2:colnum], as.numeric) 
  
  #similar for missed concerts
  accountsMissedPreferences = melt(missedPerAccountWithConcerts, id = c("account.id", "season"))
  accountsMissedPreferences = dcast(accountsMissedPreferences, account.id~variable, value.var="value", fun.aggregate = sum)
  
  missedPerAccountConcertsAdjustedTo2014Concerts = accountsMissedPreferences[,c("account.id", "BACH", "HANDEL", "TELEMAN", "JOHANN", "VIVALDI", "HAYDN", "ROSSINI")]
  # should we inflate that by times the concert is happening in 2014?
  colnum = ncol(missedPerAccountConcertsAdjustedTo2014Concerts)
  colnames(missedPerAccountConcertsAdjustedTo2014Concerts)[2:colnum] = paste("conc_missed", colnames(missedPerAccountConcertsAdjustedTo2014Concerts)[2:colnum], sep = "_")
  missedPerAccountConcertsAdjustedTo2014Concerts[2:colnum] = sapply(missedPerAccountConcertsAdjustedTo2014Concerts[2:colnum], as.numeric) 
  
  #############################
  # end parsing concerts table#
  #############################
  
  # numerics...  
  subsTrain = data$subscriptions[which(data$subscriptions$season != "2014-2015"),]
  subsTrainLong = melt(subsTrain,id=c('account.id','season'))
  subsTrainWide = dcast(subsTrainLong, account.id~variable+season,value.var="value")
  
  subsTrainWide = merge(data$accounts, subsTrainWide, by = "account.id", all.x = TRUE)
  subsTrainWide = merge(subsTrainWide, ticketsPerSeason, by = "account.id", all.x = TRUE)
  subsTrainWide = merge(subsTrainWide, seatsPerSeason, by = "account.id", all.x = TRUE)
  subsTrainWide = merge(subsTrainWide, ticketsWide, by = "account.id", all.x = TRUE)
  subsTrainWide = merge(subsTrainWide, accountsPreferencesAdjustedTo2014Concerts, by = "account.id", all.x = TRUE)
  subsTrainWide = merge(subsTrainWide, missedPerAccountConcertsAdjustedTo2014Concerts, by = "account.id", all.x = TRUE)
  subsTrainWide[is.na(subsTrainWide)] = 0

  # factors...
  factorsSubsTrain = data$subscriptionsFactors[which(data$subscriptionsFactors$season != "2014-2015"),]
  factorsSubsTrainLong = melt(factorsSubsTrain,id=c('account.id','season'))
  factorsSubsTrainWide = dcast(factorsSubsTrainLong, account.id~variable+season,value.var="value")
  
  subsTrainWide = merge(subsTrainWide, factorsSubsTrainWide, by = "account.id", all.x = TRUE) 
  subsTrainWide = merge(subsTrainWide, data$accountsFactor, by = "account.id", all.x = TRUE) 
  
  # fixing hyphens in names
  names(subsTrainWide) = sapply(names(subsTrainWide), str_replace, "-", "_")

  # post-processing for additional variables based on data  
  subsTrainWide$was.2013_2014.subscription.outside.city = sapply((subsTrainWide$billing.city == subsTrainWide$location_2013_2014) * (subsTrainWide$total_2013_2014 > 0), as.numeric)
  subsTrainWide$was.2013_2014.subscription.outside.city[is.na(subsTrainWide$was.2013_2014.subscription.outside.city)] = 0
  
  subsTrainWide$was.2012_2013.subscription.outside.city = sapply((subsTrainWide$billing.city == subsTrainWide$location_2012_2013) * (subsTrainWide$total_2012_2013 > 0), as.numeric)
  subsTrainWide$was.2012_2013.subscription.outside.city[is.na(subsTrainWide$was.2012_2013.subscription.outside.city)] = 0
  
  subsTrainWide$was.2011_2012.subscription.outside.city = sapply((subsTrainWide$billing.city == subsTrainWide$location_2011_2012) * (subsTrainWide$total_2011_2012 > 0), as.numeric)
  subsTrainWide$was.2011_2012.subscription.outside.city[is.na(subsTrainWide$was.2011_2012.subscription.outside.city)] = 0
  
  subsTrainWide$was.2010_2011.subscription.outside.city = sapply((subsTrainWide$billing.city == subsTrainWide$location_2010_2011) * (subsTrainWide$total_2010_2011 > 0), as.numeric)
  subsTrainWide$was.2010_2011.subscription.outside.city[is.na(subsTrainWide$was.2010_2011.subscription.outside.city)] = 0
  
  # fixing character columns to factors
  i <- sapply(subsTrainWide, is.character)
  i[c("account.id")] = FALSE
  subsTrainWide[i] <- lapply(subsTrainWide[i], as.factor)
  
  # remove filtered data from the set
  if(nchar(filterRegex) > 0) {
    subsTrainWide = subsTrainWide[,-grep(filterRegex, colnames(subsTrainWide))]
  }
  
  # prepare training, test sets, splits
  trainPlusTotal = merge(data$train, subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
  trainPlusTotal[is.na(trainPlusTotal)]=0

  validationRowsNums = sample(nrow(trainPlusTotal), nrow(trainPlusTotal)*validationRatio)
  validationRowsNums = sort(validationRowsNums)
  
  isValidationRow=rep(FALSE, nrow(trainPlusTotal))
  for(i in seq(1, nrow(trainPlusTotal))) {
    isValidationRow[validationRowsNums[i]]=TRUE  
  }
  
  trainPlusTotalMinusValidation = trainPlusTotal[!isValidationRow,]
  trainPlusTotalMinusValidationAndMinusAccountId=trainPlusTotalMinusValidation[2:ncol(trainPlusTotalMinusValidation)]
  
  validationTotal = trainPlusTotal[isValidationRow,]
  validationMinusTotalAndMinusAccountId=validationTotal[3:ncol(validationTotal)]
  correctAnswers = validationTotal$total

  trainPlusTotalAndMinusAccountId=trainPlusTotal[2:ncol(trainPlusTotal)]
  
  return (list("testSetAll"=validationTotal, "allSetAll"=trainPlusTotal, "allSet"=trainPlusTotalAndMinusAccountId ,"trainSet"=trainPlusTotalMinusValidationAndMinusAccountId, "testSet"=validationMinusTotalAndMinusAccountId, "testAnswers"=correctAnswers, "predictors"=subsTrainWide))
}

cleanData = function (data) {
  
  # WARNING: should be done for both data$trainSet and data$allSet !

  #high residuals
  
  
  data$trainSet=data$trainSet[!((rownames(data$trainSet)==6031)),]
  data$allSet=data$allSet[!((rownames(data$allSet)==6031)),]

  data$trainSet=data$trainSet[!((rownames(data$trainSet)==1472)),]
  data$allSet=data$allSet[!((rownames(data$allSet)==1472)),]

  #data$trainSet=data$trainSet[!((rownames(data$trainSet)==1649)),]
  #data$allSet=data$allSet[!((rownames(data$allSet)==1649)),]

  #data$trainSet=data$trainSet[!((rownames(data$trainSet)==3736)),]
  #data$allSet=data$allSet[!((rownames(data$allSet)==3736)),]
  
  #data$trainSet=data$trainSet[!((rownames(data$trainSet)==567)),]
  #data$allSet=data$allSet[!((rownames(data$allSet)==567)),]
  
  data$trainSet=data$trainSet[!((rownames(data$trainSet)==306)),]
  data$trainSet=data$trainSet[!((rownames(data$trainSet)==363)),]
  data$trainSet=data$trainSet[!((rownames(data$trainSet)==427)),]
  
  data$allSet=data$allSet[!((rownames(data$allSet)==306)),]
  data$allSet=data$allSet[!((rownames(data$allSet)==363)),]
  data$allSet=data$allSet[!((rownames(data$allSet)==427)),]
  
  #data$trainSet = data$trainSet[ - (abs(data$trainSet$total - data$trainSet$total_2013_2014)>1.5),]
  #data$allSet = data$allSet[ - (abs(data$allSet$total - data$allSet$total_2013_2014)>1.5),]
  
  return (data)
}

prepareDataToPredict = function(allPredictors) {

  test = read.csv('data\\test.csv')  
  testMinusTotal= merge(test, allPredictors, by="account.id", all.x=TRUE, all.y=FALSE)
  testMinusTotal[is.na(testMinusTotal)]=0
  
  testMinusTotalAndAccountId = testMinusTotal[,2:ncol(testMinusTotal)]
  
  accounts=testMinusTotal$account.id;
  accounts=sapply(accounts, as.character)
  
  return (list("testSet"= testMinusTotalAndAccountId, "accounts" = accounts))
}

dumpResponse = function(prefix, accounts, predictions) {

  account.id=list()
  total=list()
  for(i in 1:length(accounts)) {
    account.id = c(account.id, accounts[[i]])
    total = c(total, adjustAnswer(predictions[i])) 
  }
  
  entry=cbind(account.id,total)
  write.csv(entry, paste(prefix, format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = FALSE)  
  
}