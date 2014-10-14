
# path to kaggle data and other files
setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")

source("config.r")
source("helpers.r")
source("data.r")

set.seed(30)
validationRatio = 0.25
filter = "199|2000|2001|2002|2003|2004|2005|2006|2007"

trees = 5000
bagfrac = 0.5
shrinkage = 0.002
depth = 4

setConfig()
data = readData()

data = preparePredictors(data, filter, validationRatio)
data = cleanData(data)

formula = prepareFormula()

gbm.orch = gbm(formula, data = data$trainSet,distribution = "gaussian", 
               bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)

summary(gbm.orch)
gbm.boost = predict(gbm.orch , newdata=data$testSet, n.trees=trees)

error = 0;
residuals = c()
answers = c()
for(i in 1:length(data$testAnswers)) {
  correctAnswer = data$testAnswers[[i]]
  answer = gbm.boost[i]
  answers = c(answers, answer)
  residuals = c(residuals, correctAnswer - answer)
  error = error + (log(answer+1)-log(correctAnswer+1))^2
}

plot(answers, residuals)

error = error / length(data$testAnswers)
error = sqrt(error)
print(paste("Error in validation set: ", error, " based on: ", length(data$testAnswers), " samples"))

gbm.orch = gbm(formula, data=data$allSet,distribution="gaussian", 
               bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)

predictSet = prepareDataToPredict(data$predictors)
predictions = predict(gbm.orch, newdata=predictSet$testSet, n.trees=trees)

dumpResponse("ML_gbm_sub", predictSet$accounts, predictions)
