
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
#setwd("~/Documents/towerProperty") # Matt's wd path


source("config.r")
source("helpers.r")
source("data.r")

source("train.lm.r")

#setConfigForMyEnvironment() # special helper function for Matt's environment
includeLibraries()

useLogTransform = FALSE
validationRatio = 0.15
filter = "199|200|add_"

rawData = readData(useLogTransform)

# we will try a few different splits with different seeds
seeds = c(234294, 340549, 879138, 188231, 646946, 160318, 853181, 551724, 398728, 323126)

# additional seeds to try
# seeds = c(seeds, 917130, 539667, 239507, 221913, 321676, 153055, 804996, 903034, 52264, 587371)

formula = prepareFormula(useLogTransform)

testError = 0
for(seed in seeds) {
  set.seed(seed)

  data = preparePredictors(rawData, filter, validationRatio)
  data = cleanData(data)
  
  model = trainLMModel(formula, data$trainSet)
  summary(model)
  debugLMFit(model)
  predictions = predictFromLMModel(model, data$testSet)
  testError = testError + evaluateModel(predictions, data$testAnswers, useLogTransform)
}

tries = length(seeds)
print(paste("Final test error=", testError / tries, " based on ", tries, " tries"))

debugFitAndResiduals(predictions, data$testAnswers)

# re-train with whole set
model= trainLMModel(formula, data$allSet)
#debugFit(model)

predictSet = prepareDataToPredict(data$predictors)
predictions = predictFromLMModel(model, predictSet$testSet)

# we need to revert log(y+1) modification
if(useLogTransform) { 
  predictions = exp(predictions)-1
}

dumpResponse("ML_lm_sub_", predictSet$accounts, predictions)
