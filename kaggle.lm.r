##########
# CONFIG #
##########

# path to kaggle data
setwd("C:\\Users\\mlewo_000\\SkyDrive\\Documents\\stats202\\kaggle")

# ratio of file used for validation
validationRatio = 0.25
answerIncreaseFactor = 0.05
debug = FALSE

set.seed(1)

###########
# HELPERS #
###########

adjustAnswer = function(score) {
  
  if(score < 0.1) {
    return (0)
  }
  return (score)
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
names(subsTrainWide)=sapply(names(subsTrainWide), str_replace, "-", "_")

trainWithTotal = merge(train,subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
trainWithTotal[is.na(trainWithTotal)]=0

validationRowsNums = sample(nrow(trainWithTotal), dim(trainWithTotal)[1]*validationRatio)
validationRowsNums = sort(validationRowsNums)

isValidationRow=rep(FALSE, dim(trainWithTotal)[1])
for(i in seq(1, dim(trainWithTotal)[1])) {
  isValidationRow[validationRowsNums[i]]=TRUE  
}

trainWithTotalMinusValidation = trainWithTotal[!isValidationRow,]
trainWithTotalMinusValidationAndWithoutAccountId=trainWithTotalMinusValidation[2:65]

##################
# TRAINING MODEL #
##################

#polynomial order
polyOrder = 1

formula = as.formula(
  total ~ . + 
    # polynomials for total subs in last 3 years
    #poly(total_2011_2012, polyOrder) + 
    #poly(total_2012_2013, polyOrder) + 
    #poly(total_2013_2014, polyOrder) + 

    # log polynomials for total subs in last 3 years
    poly(log(1+total_2011_2012), polyOrder) +
    poly(log(1+total_2012_2013), polyOrder) +
    poly(log(1+total_2013_2014), polyOrder)
    
    # intercations between price levels and total_subscriptions
    poly(price.level_2011_2012*total_2011_2012, polyOrder) + 
    poly(price.level_2012_2013*total_2012_2013, polyOrder) + 
    poly(price.level_2013_2014*total_2013_2014, polyOrder)
  
    # intercations between no.seats and price_levels
    #poly(price.level_2011_2012*no.seats_2011_2012, polyOrder) + 
    #poly(price.level_2012_2013*no.seats_2012_2013, polyOrder) + 
    #poly(price.level_2013_2014*no.seats_2013_2014, polyOrder)
)

lm.fit = lm(formula, data=trainWithTotalMinusValidationAndWithoutAccountId)

if(debug) {
  summary(lm.fit)
  par(mfrow=c(2,2))
  plot(lm.fit)
}

###############################################################
# VERIFICATION BASED ON PART OF TRAINING SET (VALIDATION SET) #
###############################################################

validationTotal = trainWithTotal[isValidationRow,]
validationWithoutTotalAndWithoutAccountId=validationTotal[3:65]
correctAnswers = validationTotal$total
pr = predict(lm.fit, newdata=validationWithoutTotalAndWithoutAccountId,type="response")

# compute validatation test error
error = 0;
for(i in 1:length(correctAnswers)) {
  correctAnswer = correctAnswers[[i]]
  answer = adjustAnswer(pr[i])
  
  error = error + (log(answer+1)-log(correctAnswer+1))^2
}

error = error / length(correctAnswers)
error = sqrt(error)
print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))

################################################
# RETRAIN MODEL WITH ALL DATA AND SAME FORMULA #
################################################

#trainWithTotalAndWithoutAccountId=trainWithTotal[2:65]
#lm.fit = lm(formula, data=trainWithTotalAndWithoutAccountId)
#if(debug) {
#  summary(lm.fit)
#}

######################
# DUMPING FINAL FILE #
######################

testWithoutTotal= merge(test,subsTrainWide,by="account.id",all.x=TRUE, all.y=FALSE)
testWithoutTotal[is.na(testWithoutTotal)]=0
dim(testWithoutTotal)

testWithoutTotalAndAccountId = testWithoutTotal[,2:64]
pr = predict(lm.fit, newdata=testWithoutTotalAndAccountId,type="response")

testacc=testWithoutTotal$account.id;
testacc=sapply(testacc, as.character)
account.id=list()
total=list()
for(i in 1:length(testacc)) {
  account.id = c(account.id, testacc[[i]])
  total = c(total, adjustAnswer(pr[i])) 
}

entry=cbind(account.id,total)
write.csv(entry, paste("ML_sub_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = FALSE)

