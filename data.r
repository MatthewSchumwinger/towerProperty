
readData = function(useLogTransform) {
  
  train = read.csv('data\\train.csv')
  
  if(useLogTransform) {
    train$total = log(1+train$total)  
  }
  
  subscriptions = read.csv('data\\subscriptions.csv',colClasses='character')
  subscriptions = subscriptions[,c("account.id", "price.level", "no.seats", "total", "season" )]

  accounts = read.csv('data\\account.csv',colClasses='character') #Kartik added
  accounts = accounts[,c("account.id", "amount.donated.2013", "amount.donated.lifetime","no.donations.lifetime")] #Kartik added
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
  
  data$accounts[2:4] = sapply(data$accounts[2:4], as.numeric) 
  
  #additional data based on accounts
  data$accounts$add_donated.2013 = sapply((data$accounts$amount.donated.2013 > 0), as.numeric)
  data$accounts$add_no.donations.lifetime.if.donated.2013 = data$accounts$add_donated.2013 * data$accounts$no.donations.lifetime

  #avg donation
  data$accounts$add_avg.donation = ifelse(data$accounts$no.donations.lifetime==0, 0, data$accounts$amount.donated.lifetime / data$accounts$no.donations.lifetime)
  #avg donation if donated last year
  data$accounts$add_avg.donation.if.donated.2013 = data$accounts$add_donated.2013 * data$accounts$add_avg.donation
  
  data$subscriptions[is.na(data$subscriptions)]=0
  data$subscriptions[2:4] = sapply(data$subscriptions[2:4], as.numeric)

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
  concertsInSeasonsWithoutFreq = dcast(concertsInSeasons, season~variable, value.var = "value", fun.aggregate = max)
  
  totalPerAccount = data$subscriptions[which(data$subscriptions$season != "2014-2015"),]
  totalPerAccount = totalPerAccount[,c("account.id", "total", "season")]
  
  #select either concertsInSeasonsWithFreq or concertsInSeasonsWithoutFreq for weighted on unwieghted version
  totalPerAccountWithConcerts = merge(totalPerAccount, concertsInSeasonsWithFreq, by="season")
  
  #comment that line if you don't want to multiply by number of subscriptions bought that year
  totalPerAccountWithConcerts[4:36] = totalPerAccountWithConcerts[4:36] * totalPerAccountWithConcerts$total

  # removing total variable as it is no longer needed
  totalPerAccountWithConcerts = totalPerAccountWithConcerts[,-3]

  accountsPreferences = melt(totalPerAccountWithConcerts, id = c("account.id", "season"))
  accountsPreferences = dcast(accountsPreferences, account.id~variable, value.var="value", fun.aggregate = sum)
  
  #adjusting for number of concerts in 2014 - should we filter only to those that will be played in 2014-2015?
  #accountsPreferencesAdjustedTo2014Concerts = accountsPreferences[,c("account.id", "BACH", "HANDEL", "TELEMAN", "JOHANN", "VIVALDI", "HAYDN", "ROSSINI")]
  accountsPreferencesAdjustedTo2014Concerts = accountsPreferences
  
  colnum = ncol(accountsPreferencesAdjustedTo2014Concerts)
  colnames(accountsPreferencesAdjustedTo2014Concerts)[2:colnum] = paste("conc", colnames(accountsPreferencesAdjustedTo2014Concerts)[2:colnum], sep = "_")
  accountsPreferencesAdjustedTo2014Concerts[2:colnum] = sapply(accountsPreferencesAdjustedTo2014Concerts[2:colnum], as.numeric) 
  #############################
  # end parsing concerts table#
  #############################
  
  
  subsTrain = data$subscriptions[which(data$subscriptions$season != "2014-2015"),]
  subsTrainLong = melt(subsTrain,id=c('account.id','season'))
  subsTrainWide = dcast(subsTrainLong, account.id~variable+season,value.var="value")
  subsTrainWide = merge(subsTrainWide, data$accounts,by = "account.id") 

  subsTrainWide = merge(subsTrainWide, ticketsPerSeason, by = "account.id", all.x = TRUE)
  subsTrainWide = merge(subsTrainWide, seatsPerSeason, by = "account.id", all.x = TRUE)
  
  subsTrainWide = merge(subsTrainWide, ticketsWide, by = "account.id", all.x = TRUE)

  subsTrainWide = merge(subsTrainWide, accountsPreferencesAdjustedTo2014Concerts, by = "account.id", all.x = TRUE)
  
  
  # fixing hyphens in names
  names(subsTrainWide) = sapply(names(subsTrainWide), str_replace, "-", "_")
  
  # remove old data from the set
  if(nchar(filterRegex) > 0) {
    subsTrainWide = subsTrainWide[,-grep(filterRegex, colnames(subsTrainWide))]
  }
  
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