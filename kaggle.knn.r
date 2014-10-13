##########
# CONFIG #
##########

# path to kaggle data
setwd("C:\\Users\\mlewo_000\\SkyDrive\\Documents\\stats202\\kaggle")

# ratio of file used for validation
validationRatio = 0.25
zeroThreshold = 0.0
debug = FALSE
dropOldData = TRUE

set.seed(30)

###########
# HELPERS #
###########

adjustAnswer = function(score) {
  
  if(score < zeroThreshold) {
    return (0)
  }
  return (score)
  #answerIncreaseFactor=0.05
  #return (round(score*2 + answerIncreaseFactor)/2)
}

################
# MAIN PROGRAM #
################

library(reshape2)
library(stringr)
test = read.csv('test.csv')
train = read.csv('train.csv')

################
# READING DATA #
################

subscriptions = read.csv('subscriptions.csv',colClasses='character')
subscriptions = subscriptions[,c("account.id", "price.level", "no.seats", "total", "season" )]

########################
# PREPARING PREDICTORS #
########################

subscriptions[is.na(subscriptions)]=0
subscriptions[2:4] = sapply(subscriptions[2:4], as.numeric)

subsTrain = subscriptions[which(subscriptions$season != "2014-2015"),]

subsTrainLong = melt(subsTrain,id=c('account.id','season'))
subsTrainWide = dcast(subsTrainLong, account.id~variable+season,value.var="value")

# fixing hyphens in names
names(subsTrainWide) = sapply(names(subsTrainWide), str_replace, "-", "_")

# remove old data from the set
if(dropOldData) {
  subsTrainWide = subsTrainWide[,-grep("199|200|price|2010", colnames(subsTrainWide))]
}

trainPlusTotal = merge(train,subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
trainPlusTotal[is.na(trainPlusTotal)]=0

# factorizing total for knn
#trainPlusTotal$total = as.factor(trainPlusTotal$total)

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
##################
# TRAINING MODEL #
##################

# KNN, beware it is adding random vector to the data to overcome "too many ties" error so you need 
# to always re-run from scratch after running it :/
if(FALSE) {
  library(class)
  
  for(k in seq (1, 15)) {
  trainPlusTotalMinusValidationAndMinusAccountId$rand = rnorm(nrow(trainPlusTotalMinusValidationAndMinusAccountId))/10000000
  validationMinusTotalAndMinusAccountId$rand = rnorm(nrow(validationMinusTotalAndMinusAccountId))/10000000
  
  train.X = as.matrix(trainPlusTotalMinusValidationAndMinusAccountId[2:ncol(trainPlusTotalMinusValidationAndMinusAccountId)])
  test.X = as.matrix(validationMinusTotalAndMinusAccountId)
  train.Total = trainPlusTotalMinusValidationAndMinusAccountId$total
  knn.pred = knn(train.X, test.X, train.Total, k=k)
  #table(knn.pred, correctAnswers)
  
  error = 0;
  for(i in 1:length(correctAnswers)) {
    correctAnswer = as.numeric(as.character(correctAnswers[[i]]))
    answer = as.numeric(as.character(knn.pred[i]))
    error = error + (log(answer+1)-log(correctAnswer+1))^2
  }
  
  error = error / length(correctAnswers)
  error = sqrt(error)
  print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))
  }
}

polyOrder = 2


formula = as.formula(
  total ~ . #+
    #-total_2011_2012 -total_2012_2013 -total_2013_2014 +
    # polynomials for total subs in last 3 years
    #poly(total_2011_2012, polyOrder) 
    #poly(total_2012_2013, polyOrder) + 
    #poly(total_2013_2014, polyOrder) +
    
    # log polynomials for total subs in last 3 years
    #poly(log(1+total_2011_2012), polyOrder) +
    #poly(log(1+total_2012_2013), polyOrder) +
    #poly(log(1+total_2013_2014), polyOrder)

    # intercations between price levels and total_subscriptions
    #poly(log(1+price.level_2011_2012*total_2011_2012), polyOrder) + 
    #poly(log(1+price.level_2012_2013*total_2012_2013), polyOrder) + 
    #poly(log(1+price.level_2013_2014*total_2013_2014), polyOrder) +
    
    # intercations between no.seats and price_levels
    #poly(log(1+price.level_2011_2012*no.seats_2011_2012), polyOrder) + 
    #poly(log(1+price.level_2012_2013*no.seats_2012_2013), polyOrder) + 
    #poly(log(1+price.level_2013_2014*no.seats_2013_2014), polyOrder)
)


gbm.orch = gbm(formula, data=trainPlusTotalMinusValidationAndMinusAccountId,distribution="gaussian", 
                bag.fraction = 0.7, shrinkage=0.005, n.trees = 500, interaction.depth=4)
summary(gbm.orch)
gbm.boost = predict(gbm.orch , newdata= validationMinusTotalAndMinusAccountId,n.trees=500)

error = 0;
for(i in 1:length(correctAnswers)) {
  correctAnswer = correctAnswers[[i]]
  answer = gbm.boost[i]
  error = error + (log(answer+1)-log(correctAnswer+1))^2
}

error = error / length(correctAnswers)
error = sqrt(error)
print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))


# multi-class logistic regression
if(TRUE) {
  library(nnet)
  multi.fit = multinom(formula , data = trainPlusTotalMinusValidationAndMinusAccountId)
  summary(multi.fit)
  
  predictions = predict(multi.fit, validationMinusTotalAndMinusAccountId)
  table(predictions, correctAnswers)
  
  error = 0;
  for(i in 1:length(correctAnswers)) {
    correctAnswer = as.numeric(as.character(correctAnswers[[i]]))
    answer = as.numeric(as.character(predictions[i]))
    error = error + (log(answer+1)-log(correctAnswer+1))^2
  }
  
  error = error / length(correctAnswers)
  error = sqrt(error)
  print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))
}




error = 0;
for(i in 1:length(correctAnswers)) {
  correctAnswer = as.numeric(as.character(correctAnswers[[i]]))
  answer = as.numeric(as.character(lda.predTest$class[i]))
  error = error + (log(answer+1)-log(correctAnswer+1))^2
}

error = error / length(correctAnswers)
error = sqrt(error)
print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))

if(debug) {
  errorRows = lda.predTest$class != correctAnswers
  mistakes = 
    cbind(validationMinusTotalAndMinusAccountId[errorRows, c("total_2013_2014")],
    correctAnswers[errorRows],
    lda.predTest$class[errorRows])
  mistakes
}



######################
# DUMPING FINAL FILE #
######################

testMinusTotal= merge(test,subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
testMinusTotal[is.na(testMinusTotal)]=0

testMinusTotalAndAccountId = testMinusTotal[,2:ncol(testMinusTotal)]
lda.predTest = predict(lda.fit, testMinusTotalAndAccountId)

testacc=testMinusTotal$account.id;
testacc=sapply(testacc, as.character)
account.id=list()
total=list()
for(i in 1:length(testacc)) {
  account.id = c(account.id, testacc[[i]])
  total = c(total, as.numeric(as.character(lda.predTest$class[i])))
}

entry=cbind(account.id,total)
write.csv(entry, paste("ML_lda_sub_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = FALSE)
