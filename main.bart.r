
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
#setwd("~/Documents/towerProperty") # Matt's wd path

source("config.r")
source("helpers.r")
source("data.r")

library(BayesTree)

setConfigForMyEnvironment() # special helper function for Matt's environment

includeLibraries()

filter = "" 
rawData = readData(FALSE)
allPredictors = preparePredictors(rawData, filter)
allData = prepareSplits(rawData, allPredictors, c(0))

filter = "199|200|2010|price.level|add_no|TELEMAN|JOHANN|ROSSINI|conc_missed|add_price|add_tickets|add_tickets_seats|section_2013_2014|multiple.subs|billing.city|is.us|relationship|outside|City|State|Lat|Long" 

useLogTransform = FALSE 
sigdf = 3
sigquant = 0.9
k = 2.0
power=2.0
base=0.95
binaryOffset=0
ntree=300
ndpost=1000 
nskip=100
numfolds = 10

rawData = readData(useLogTransform)

set.seed(551724)
folds = sample(1:numfolds, nrow(allData$allSet), replace=T)

predictors = preparePredictors(rawData, filter)

testError = 0
testErrorInact = 0
testErrorVar = 0
for(i in 1:numfolds) {
  
  data = prepareSplits(rawData, predictors, which(folds == i))
  
  bartobj = bart(data$trainSet[,-1], data$trainSet$total, data$testSet, 
                sigdf=sigdf, sigquant=sigquant, k=k, power=power, base=base, 
                binaryOffset=binaryOffset, ntree=ntree, ndpost=ndpost, nskip=nskip, verbose=FALSE)
  
  bart.pred = bartobj$yhat.test.mean
  
  print("Raw prediction")
  testError = testError + evaluateModel(bart.pred, data$testAnswers, useLogTransform)
  
  print("Adjusting for inactive")
  adjusted = adjustPredictionsInactive(bart.pred, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorInact = testErrorInact + evaluateModel(adjusted, data$testAnswers, useLogTransform)
  
  print("Adjusting for invariance")
  adjusted2 = adjustPredictionsInvariant(bart.pred, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorVar = testErrorVar + evaluateModel(adjusted2, data$testAnswers, useLogTransform)
  
}

#tries = length(seeds)
tries = numfolds
print(paste("Final test error raw prediction=", testError / tries, " based on ", tries, " tries"))
print(paste("Final test error with inactive adj=", testErrorInact / tries, " based on ", tries, " tries"))
print(paste("Final test error with no variance adj =", testErrorVar / tries, " based on ", tries, " tries"))


predictSet = prepareDataToPredict(data$predictors)
predictSetAll = prepareDataToPredict(allData$predictors)

bartobj = bart(data$allSet[,-1], data$allSet$total, predictSet$testSet, 
               sigdf=sigdf, sigquant=sigquant, k=k, power=power, base=base, 
               binaryOffset=binaryOffset, ntree=ntree, ndpost=ndpost, nskip=nskip)

bart.pred = bartobj$yhat.test.mean

predictions = bart.pred
predictions = adjustPredictionsInactive(predictions, data.frame("account.id"=predictSet$accounts), 
                                        predictSetAll$testSetAll)

if(useLogTransform) {
  predictions = exp(predictions)-1
}

dumpResponse("ML_bart_sub", predictSet$accounts, predictions)
