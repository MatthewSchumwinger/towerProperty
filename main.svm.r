
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
#setwd("~/Documents/towerProperty") # Matt's wd path

source("config.r")
source("helpers.r")
source("data.r")

library(e1071)

setConfigForMyEnvironment() # special helper function for Matt's environment
includeLibraries()


filter = "" 
rawData = readData(FALSE)
allPredictors = preparePredictors(rawData, filter)
allData = prepareSplits(rawData, allPredictors, c(0))

filter = "199|200|2010|price.level|add_no|TELEMAN|JOHANN|ROSSINI|conc_missed|add_price|add_tickets|add_tickets_seats|section_2013_2014|multiple.subs|billing.city|is.us|relationship|outside|City|State|Lat|Long|package|section|location" 
useLogTransform = FALSE 
kernel="radial"
cost=1000
gamma=1
numfolds = 10

rawData = readData(useLogTransform)

polyOrder = 2
formula = prepareFormula(useLogTransform)

set.seed(551724)
folds = sample(1:numfolds, nrow(allData$allSet), replace=T)

predictors = preparePredictors(rawData, filter)

testError = 0
testErrorInact = 0
testErrorVar = 0
for(i in 1:numfolds) {
  
  data = prepareSplits(rawData, predictors, which(folds == i))

  svm.fit = svm(formula, data=data$trainSet, kernel=kernel, cost=cost, gamma=gamma)
  svm.pred = predict(svm.fit, newdata=data$testSet)
  
  print("Raw prediction")
  testError = testError + evaluateModel(svm.pred, data$testAnswers, useLogTransform)
  
  print("Adjusting for inactive")
  adjusted = adjustPredictionsInactive(svm.pred, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorInact = testErrorInact + evaluateModel(adjusted, data$testAnswers, useLogTransform)
  
  print("Adjusting for invariance")
  adjusted2 = adjustPredictionsInvariant(svm.pred, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorVar = testErrorVar + evaluateModel(adjusted2, data$testAnswers, useLogTransform)
  
}

tries = numfolds
print(paste("Final test error raw prediction=", testError / tries, " based on ", tries, " tries"))
print(paste("Final test error with inactive adj=", testErrorInact / tries, " based on ", tries, " tries"))
print(paste("Final test error with no variance adj =", testErrorVar / tries, " based on ", tries, " tries"))



svm.fit = svm(formula, data=data$allSet, kernel=kernel, cost=cost, gamma=gamma)

predictSet = prepareDataToPredict(data$predictors)
predictSetAll = prepareDataToPredict(allData$predictors)
predictions = predict(svm.fit, newdata=predictSet$testSet, n.trees=trees)

predictions = adjustPredictionsInactive(predictions, data.frame("account.id"=predictSet$accounts), 
                                        predictSetAll$testSetAll)

if(useLogTransform) {
  predictions = exp(predictions)-1
}

dumpResponse("ML_svm_sub", predictSet$accounts, predictions)
