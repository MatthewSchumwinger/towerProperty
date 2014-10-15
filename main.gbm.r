
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")

source("config.r")
source("helpers.r")
source("data.r")

validationRatio = 0.15
filter = "199|2000|2001|2002|2003|2004|2005|2006|2007"

useLogTransform = FALSE
trees = 1000
bagfrac = 0.7
shrinkage = 0.005
depth = 4

includeLibraries()
rawData = readData(useLogTransform)

# for some reasom gbm is not picking that up in the function
polyOrder = 1
formula = prepareFormula(useLogTransform)

seeds = c(234294, 340549, 879138, 188231, 646946, 160318, 853181, 551724, 398728, 323126)

testError = 0
for(seed in seeds) {
  
  set.seed(seed)
  data = preparePredictors(rawData, filter, validationRatio)
  data = cleanData(data)
  
  gbm.orch = gbm(formula, data = data$trainSet, distribution = "gaussian", 
                 bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)
  
  summary(gbm.orch)
  gbm.boost = predict(gbm.orch , newdata=data$testSet, n.trees=trees)
  
  testError = testError + evaluateModel(gbm.boost, data$testAnswers, useLogTransform)
}

tries = length(seeds)
print(paste("Final test error=", testError / tries, " based on ", tries, " tries"))

gbm.orch = gbm(formula, data=data$allSet,distribution="gaussian", 
               bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)

predictSet = prepareDataToPredict(data$predictors)
predictions = predict(gbm.orch, newdata=predictSet$testSet, n.trees=trees)

if(useLogTransform) {
  predictions = exp(predictions)-1
}

dumpResponse("ML_gbm_sub", predictSet$accounts, predictions)
