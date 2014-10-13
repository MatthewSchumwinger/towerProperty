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

set.seed(1)

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
  subsTrainWide = subsTrainWide[,-grep("199|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010", colnames(subsTrainWide))]
}

trainPlusTotal = merge(train,subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
trainPlusTotal[is.na(trainPlusTotal)]=0

validationRowsNums = sample(nrow(trainPlusTotal), nrow(trainPlusTotal)*validationRatio)
validationRowsNums = sort(validationRowsNums)

isValidationRow=rep(FALSE, nrow(trainPlusTotal))
for(i in seq(1, nrow(trainPlusTotal))) {
  isValidationRow[validationRowsNums[i]]=TRUE  
}

trainPlusTotalMinusValidation = trainPlusTotal[!isValidationRow,]
trainPlusTotalMinusValidationAndMinusAccountId=trainPlusTotalMinusValidation[2:ncol(trainPlusTotalMinusValidation)]

#remove points wih high residuals and high leverage

#high residuals
trainPlusTotalMinusValidationAndMinusAccountId=trainPlusTotalMinusValidationAndMinusAccountId[!((rownames(trainPlusTotalMinusValidationAndMinusAccountId)==6031)),]
#trainPlusTotalMinusValidationAndMinusAccountId=trainPlusTotalMinusValidationAndMinusAccountId[!((rownames(trainPlusTotalMinusValidationAndMinusAccountId)==363)),]
#trainPlusTotalMinusValidationAndMinusAccountId=trainPlusTotalMinusValidationAndMinusAccountId[!((rownames(trainPlusTotalMinusValidationAndMinusAccountId)==306)),]

#high leverage
#trainPlusTotalMinusValidationAndMinusAccountId=trainPlusTotalMinusValidationAndMinusAccountId[!((rownames(trainPlusTotalMinusValidationAndMinusAccountId)==427)),]

##################
# TRAINING MODEL #
##################

#polynomial order
polyOrder = 1

formula = as.formula(
  total ~ . 
    #+
    # polynomials for total subs in last 3 years
    #poly(total_2011_2012, polyOrder) + 
    #poly(total_2012_2013, polyOrder) + 
    #poly(total_2013_2014, polyOrder) +

    # log polynomials for total subs in last 3 years
    #poly(log(1+total_2011_2012), polyOrder) +
    #poly(log(1+total_2012_2013), polyOrder) +
    #poly(log(1+total_2013_2014), polyOrder) 
    
    # intercations between price levels and total_subscriptions
    #poly(price.level_2011_2012*total_2011_2012, polyOrder) + 
    #poly(price.level_2012_2013*total_2012_2013, polyOrder) + 
    #poly(price.level_2013_2014*total_2013_2014, polyOrder) +

    #poly(log(1+price.level_2012_2013*total_2012_2013), polyOrder) + 
    #poly(log(1+price.level_2013_2014*total_2013_2014), polyOrder)
  
    # intercations between no.seats and price_levels
    #poly(price.level_2011_2012*no.seats_2011_2012, polyOrder) + 
    #poly(price.level_2012_2013*no.seats_2012_2013, polyOrder) + 
    #poly(price.level_2013_2014*no.seats_2013_2014, polyOrder)
)

lm.fit = lm(formula, data=trainPlusTotalMinusValidationAndMinusAccountId)

if(debug) {
  summary(lm.fit)
  par(mfrow=c(2,2))
  plot(lm.fit)
}

###############################################################
# VERIFICATION BASED ON PART OF TRAINING SET (VALIDATION SET) #
###############################################################

validationTotal = trainPlusTotal[isValidationRow,]
validationMinusTotalAndMinusAccountId=validationTotal[3:ncol(validationTotal)]
correctAnswers = validationTotal$total
pr = predict(lm.fit, newdata=validationMinusTotalAndMinusAccountId,type="response")

# compute validatation test error
error = 0;
residuals = c()
answers = c()
for(i in 1:length(correctAnswers)) {
  correctAnswer = correctAnswers[[i]]
  answer = adjustAnswer(pr[i])
  residuals = c(residuals, correctAnswer-answer)
  answers = c(answers, answer)
  error = error + (log(answer+1)-log(correctAnswer+1))^2
}

if(debug) {
  par(mfrow=c(1,1))
  plot(correctAnswers, residuals)
  plot(answers, residuals)
  plot(correctAnswers, answers)
}

error = error / length(correctAnswers)
error = sqrt(error)
print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))

################################################
# RETRAIN MODEL Plus ALL DATA AND SAME FORMULA #
################################################

trainPlusTotalAndMinusAccountId=trainPlusTotal[2:ncol(trainPlusTotal)]
trainPlusTotalAndMinusAccountId=trainPlusTotalAndMinusAccountId[!((rownames(trainPlusTotalAndMinusAccountId)==6031)),]
lm.fit = lm(formula, data=trainPlusTotalAndMinusAccountId)

if(debug) {
  plot(lm.fit)
  summary(lm.fit)
}

######################
# DUMPING FINAL FILE #
######################

testMinusTotal= merge(test,subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
testMinusTotal[is.na(testMinusTotal)]=0

testMinusTotalAndAccountId = testMinusTotal[,2:ncol(testMinusTotal)]
pr = predict(lm.fit, newdata=testMinusTotalAndAccountId,type="response")

testacc=testMinusTotal$account.id;
testacc=sapply(testacc, as.character)
account.id=list()
total=list()
for(i in 1:length(testacc)) {
  account.id = c(account.id, testacc[[i]])
  total = c(total, adjustAnswer(pr[i])) 
}

entry=cbind(account.id,total)
write.csv(entry, paste("ML_lm_sub_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = FALSE)

