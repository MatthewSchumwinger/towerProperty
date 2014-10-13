
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")

source("config.r")
source("helpers.r")
source("data.r")

source("train.lm.r")

validationRatio = 0.25
filter = "199|200|2010"

setConfig()
data = readData()

data = preparePredictors(data, filter, validationRatio)
data = cleanData(data)

formula = prepareFormula()

model = trainLMModel(formula, data$trainSet)

debugLMFit(model)

predictions = predictFromLMModel(model, data$testSet)
error = evaluateModel(predictions, data$testAnswers)

debugFitAndResiduals(predictions, data$testAnswers)

# re-train with whole set
model= trainLMModel(formula, data$allSet)
#debugFit(model)

predictSet = prepareDataToPredict(data$predictors)
predictions = predictFromLMModel(model, predictSet$testSet)

dumpResponse("ML_lm_sub", predictSet$accounts, predictions)
