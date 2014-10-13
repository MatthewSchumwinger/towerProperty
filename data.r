
readData = function() {
  
  train = read.csv('data\\train.csv')

  subscriptions = read.csv('data\\subscriptions.csv',colClasses='character')
  subscriptions = subscriptions[,c("account.id", "price.level", "no.seats", "total", "season" )]

  accounts = read.csv('data\\account.csv',colClasses='character') #Kartik added
  accounts = accounts[,c("account.id", "amount.donated.2013", "amount.donated.lifetime","no.donations.lifetime")] #Kartik added
  # next step "billing.zip.code"
  
  return (list("train"=train, "subscriptions"=subscriptions, "accounts"=accounts))
}

preparePredictors = function(data, filterRegex, validationRatio) {
  
  data$accounts[2:4] = sapply(data$accounts[2:4], as.numeric) 
  data$subscriptions[is.na(data$subscriptions)]=0
  
  data$subscriptions[2:4] = sapply(data$subscriptions[2:4], as.numeric)
  subsTrain = data$subscriptions[which(data$subscriptions$season != "2014-2015"),]
  subsTrainLong = melt(subsTrain,id=c('account.id','season'))
  subsTrainWide = dcast(subsTrainLong, account.id~variable+season,value.var="value")
  subsTrainWide = merge(subsTrainWide, data$accounts,by = "account.id") 
  
  # fixing hyphens in names
  names(subsTrainWide) = sapply(names(subsTrainWide), str_replace, "-", "_")
  
  # remove old data from the set
  subsTrainWide = subsTrainWide[,-grep(filterRegex, colnames(subsTrainWide))]
 
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
  
  return (list("allSet"=trainPlusTotalAndMinusAccountId ,"trainSet"=trainPlusTotalMinusValidationAndMinusAccountId, "testSet"=validationMinusTotalAndMinusAccountId, "testAnswers"=correctAnswers, "predictors"=subsTrainWide))
}

cleanData = function (data) {
  
  # WARNING: should be done for both data$trainSet and data$allSet !

  #high residuals
  data$trainSet=data$trainSet[!((rownames(data$trainSet)==6031)),]
  data$allSet=data$allSet[!((rownames(data$allSet)==6031)),]
  # 363, 306 ?  
  
  #high leverage
  # 427 ?
  
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