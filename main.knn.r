
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
#setwd("~/Documents/towerProperty") # Matt's wd path

source("config.r")
source("helpers.r")
source("data.r")

library(class)

setConfigForMyEnvironment() # special helper function for Matt's environment
includeLibraries()


filter = "" 
rawData = readData(FALSE)
allPredictors = preparePredictors(rawData, filter)
allData = prepareSplits(rawData, allPredictors, c(0))

filter = "199|200|package|location|price.level|add_no|TELEMAN|JOHANN|ROSSINI|add_price|add_tickets|add_tickets_seats|section_2013_2014|multiple.subs|billing.city|is.us|relationship|outside|City|State|Lat|Long|package|section|location" 
useLogTransform = FALSE 
k = 3
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
  
  knn.pred = knn(train=data$trainSet[,-1], test=data$testSet, cl=data$trainSet$total, k = k)

  table(knn.pred, data$testAnswers)
  
  print("Raw prediction")
  testError = testError + evaluateModel(knn.pred, data$testAnswers, useLogTransform)
  
  print("Adjusting for inactive")
  adjusted = adjustPredictionsInactive(knn.pred, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorInact = testErrorInact + evaluateModel(adjusted, data$testAnswers, useLogTransform)
  
  print("Adjusting for invariance")
  adjusted2 = adjustPredictionsInvariant(knn.pred, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorVar = testErrorVar + evaluateModel(adjusted2, data$testAnswers, useLogTransform)
  
}

tries = numfolds
print(paste("Final test error raw prediction=", testError / tries, " based on ", tries, " tries"))
print(paste("Final test error with inactive adj=", testErrorInact / tries, " based on ", tries, " tries"))
print(paste("Final test error with no variance adj =", testErrorVar / tries, " based on ", tries, " tries"))


