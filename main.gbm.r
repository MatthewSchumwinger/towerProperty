
# path to kaggle data and other files
#setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
setwd("~/Documents/towerProperty") # Matt's wd path

source("config.r")
source("helpers.r")
source("data.r")

library(gbm)

setConfigForMyEnvironment() # special helper function for Matt's environment


filter = "" 
rawData = readData(FALSE)
allPredictors = preparePredictors(rawData, filter)
allData = prepareSplits(rawData, allPredictors, c(0))

#validationRatio = 0.15
#VIVALDI|HAYDN|HANDEL
filter = "199|200|2010|price.level|add_no|TELEMAN|JOHANN|ROSSINI|conc_missed|add_price|add_tickets|add_tickets_seats|section_2013_2014|multiple.subs|billing.city|is.us|relationship|outside|City|State|Lat|Long" 

useLogTransform = FALSE 
trees = 4000 
bagfrac = 0.5 
shrinkage = 0.001
depth = 4
numfolds = 10

includeLibraries()
rawData = readData(useLogTransform)

# for some reasom gbm is not picking that up in the function
polyOrder = 2
formula = prepareFormula(useLogTransform)

#seeds = c(234294, 340549, 879138, 188231, 646946, 160318, 853181, 551724, 398728, 323126)

set.seed(551724)
folds = sample(1:numfolds, nrow(allData$allSet), replace=T)

predictors = preparePredictors(rawData, filter)

testError = 0
testErrorInact = 0
testErrorVar = 0
for(i in 1:numfolds) {
  
  #set.seed(seed)
  data = prepareSplits(rawData, predictors, which(folds == i))
  # data = cleanData(data)

  gbm.orch = gbm(formula, data = data$trainSet, distribution = "gaussian", 
                 bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)
  
  summary(gbm.orch)
  gbm.boost = predict(gbm.orch , newdata=data$testSet, n.trees=trees)

  print("Raw prediction")
  testError = testError + evaluateModel(gbm.boost, data$testAnswers, useLogTransform)

  print("Adjusting for inactive")
  adjusted = adjustPredictionsInactive(gbm.boost, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorInact = testErrorInact + evaluateModel(adjusted, data$testAnswers, useLogTransform)

  print("Adjusting for invariance")
  adjusted2 = adjustPredictionsInvariant(gbm.boost, data.frame("account.id"=data$testAccounts), allData$predictors)
  testErrorVar = testErrorVar + evaluateModel(adjusted2, data$testAnswers, useLogTransform)

}

#tries = length(seeds)
tries = numfolds
print(paste("Final test error raw prediction=", testError / tries, " based on ", tries, " tries"))
print(paste("Final test error with inactive adj=", testErrorInact / tries, " based on ", tries, " tries"))
print(paste("Final test error with no variance adj =", testErrorVar / tries, " based on ", tries, " tries"))



summary(gbm.orch)

#deep check
for(i in 1:length(data$testAnswers)) {
  correctAnswer = data$testAnswers[[i]]
  answer = adjustAnswer(gbm.boost[i])
  if(abs(answer-correctAnswer) > 1) {
    print(paste("Big error for ", i, " predicted=", answer, " correct=", correctAnswer))
    print(data$testSet[i, grep("total_201", colnames(data$testSet))])
  } 
}  

print(data$allSetAll["5905",])
print(data$trainAccountsId[1487,1])
dim(data$trainAccountsId)


gbm.orch = gbm(formula, data=data$allSet,distribution="gaussian", 
               bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)

summary(gbm.orch)

predictSet = prepareDataToPredict(data$predictors)
predictSetAll = prepareDataToPredict(allData$predictors)
predictions = predict(gbm.orch, newdata=predictSet$testSet, n.trees=trees)

predictions = adjustPredictionsInactive(predictions, data.frame("account.id"=predictSet$accounts), 
                                predictSetAll$testSetAll)

if(useLogTransform) {
  predictions = exp(predictions)-1
}

dumpResponse("ML_gbm_sub", predictSet$accounts, predictions)
