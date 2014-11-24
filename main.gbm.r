
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
#setwd("~/Documents/towerProperty") # Matt's wd path

source("config.r")
source("helpers.r")
source("data.r")

library(gbm)

setConfigForMyEnvironment() # special helper function for Matt's environment
includeLibraries()

filter = "" 
rawData = readData(FALSE)
allPredictors = preparePredictors(rawData, filter)
allData = prepareSplits(rawData, allPredictors, c(0))

filter = "199|200|2010|2011|add_no|conc|TELEMAN|JOHANN|ROSSINI|add_price|conc_missed|ever.bought.subs|num.bought.ticket|add_tickets|add_tickets_seats|section_2013_2014|multiple.subs|billing.city|is.us|relationship|outside|City|State|Lat|Long|geo" 
useLogTransform = F
trees = 3000 
bagfrac = 0.5 
trainfrac = 1.0
shrinkage = 0.001
depth = 10
distrib = "tdist"
minobsinnode = 4
df = 1
numfolds = 10
clean = T

rawData = readData(useLogTransform)

# for some reasom gbm is not picking that up in the function
polyOrder = 2
formula = prepareFormula(useLogTransform)

set.seed(551724)
folds = sample(1:numfolds, nrow(allData$allSet), replace=T)

predictors = preparePredictors(rawData, filter)

testError = 0
testErrorInact = 0
testErrorVar = 0
testErrorRound = 0
trainError = 0
for(i in 1:numfolds) {
  
  #set.seed(seed)
  data = prepareSplits(rawData, predictors, which(folds == i))
  if(clean) {
    data = cleanData(data)
  }
  
  if(distrib == "tdist") {
    gbm.orch = gbm(formula, data = data$trainSet, distribution = list(name="tdist", df=df), 
                   bag.fraction = bagfrac, train.fraction = trainfrac, 
                   shrinkage = shrinkage, n.trees = trees, interaction.depth = depth, n.minobsinnode=minobsinnode)      
  } else {
    gbm.orch = gbm(formula, data = data$trainSet, distribution = distrib, 
                   bag.fraction = bagfrac, train.fraction = trainfrac, 
                   shrinkage = shrinkage, n.trees = trees, interaction.depth = depth, n.minobsinnode=minobsinnode)
  }
  
  summary(gbm.orch)
  
  print("Training error")
  gbm.train = predict(gbm.orch , newdata=data$trainSet, n.trees=trees)
  trainError = trainError + evaluateModel(gbm.train, data$trainSet$total, useLogTransform)  
  
  gbm.boost = predict(gbm.orch , newdata=data$testSet, n.trees=trees)

  print("Raw prediction")
  testError = testError + evaluateModel(gbm.boost, data$testAnswers, useLogTransform)

  print("Adjusting for inactive")
  adjusted = adjustPredictionsInactive(gbm.boost, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorInact = testErrorInact + evaluateModel(adjusted, data$testAnswers, useLogTransform)

  print("Adjusting for invariance")
  adjusted2 = adjustPredictionsInvariant(gbm.boost, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorVar = testErrorVar + evaluateModel(adjusted2, data$testAnswers, useLogTransform)

  print("Adjusting for rounding")
  adjusted3 = adjustPredictionsRound(gbm.boost, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorRound = testErrorRound + evaluateModel(adjusted3, data$testAnswers, useLogTransform)

  print("End fold")
  print("End fold")
  print("End fold")
}

tries = numfolds
print(paste("Final train error raw prediction=", trainError / tries, " based on ", tries, " tries"))
print(paste("Final test error raw prediction=", testError / tries, " based on ", tries, " tries"))
print(paste("Final test error with inactive adj=", testErrorInact / tries, " based on ", tries, " tries"))
print(paste("Final test error with no variance adj =", testErrorVar / tries, " based on ", tries, " tries"))
print(paste("Final test error with rounding =", testErrorRound / tries, " based on ", tries, " tries"))


summary(gbm.orch)

#deep check
for(i in 1:length(data$testAnswers)) {
  correctAnswer = data$testAnswers[[i]]
  answer = adjustAnswer(gbm.boost[i])
  if(abs(answer-correctAnswer) >= 1) {
    print(paste("Big error for ", i, " predicted=", answer, " correct=", correctAnswer))
    print(data$testSet[i, grep("total_201|account.num", colnames(data$testSet))])
  } 
}  

print(data$allSetAll["9900",])
print(data$trainAccountsId[1487,1])
dim(data$trainAccountsId)

if(distrib == "tdist") {
  gbm.orch = gbm(formula, data = data$allSet, distribution = list(name="tdist", df=df), 
                 bag.fraction = bagfrac, train.fraction = trainfrac, 
                 shrinkage = shrinkage, n.trees = trees, interaction.depth = depth, n.minobsinnode=minobsinnode)      
} else {
  gbm.orch = gbm(formula, data = data$allSet, distribution = distrib, 
                 bag.fraction = bagfrac, train.fraction = trainfrac, 
                 shrinkage = shrinkage, n.trees = trees, interaction.depth = depth, n.minobsinnode=minobsinnode)
}

summary(gbm.orch)

predictSet = prepareDataToPredict(data$predictors)
predictSetAll = prepareDataToPredict(allData$predictors)
predictions = predict(gbm.orch, newdata=predictSet$testSet, n.trees=trees)

#predictions = adjustPredictionsInactive(predictions, data.frame("account.id"=predictSet$accounts), 
#                                predictSetAll$testSetAll)

if(useLogTransform) {
  predictions = exp(predictions)-1
}

dumpResponse("ML_gbm_sub", predictSet$accounts, predictions)
