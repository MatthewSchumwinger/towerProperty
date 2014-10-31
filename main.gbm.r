
# path to kaggle data and other files
#setwd("C:\\Users\\mlewo_000\\Documents\\GitHub\\https---github.com-MatthewSchumwinger-towerProperty\\towerProperty")
setwd("~/Documents/towerProperty") # Matt's wd path

source("config.r")
source("helpers.r")
source("data.r")

library(gbm)

setConfigForMyEnvironment() # special helper function for Matt's environment

validationRatio = 0.15
filter = "199|200|2010|price|add_no|TELEMAN|JOHANN|PERGOLESI|HAYDN|VIVALDI|CORELLI|MOZART|ROSSINI|add_tickets|section_2013_2014|add_donated.2013|multiple.subs|package|billing.city|section|is.us|relationship|outside|City|State|Lat|Long" 

useLogTransform = FALSE 
trees = 3500 
bagfrac = 0.5 
shrinkage = 0.001
depth = 6

includeLibraries()
rawData = readData(useLogTransform)

# for some reasom gbm is not picking that up in the function
polyOrder = 2
formula = prepareFormula(useLogTransform)

seeds = c(234294, 340549, 879138, 188231, 646946, 160318, 853181, 551724, 398728, 323126)

testError = 0
for(seed in seeds) {
  
  set.seed(seed)
  data = preparePredictors(rawData, filter, validationRatio)
#  data = cleanData(data)

  gbm.orch = gbm(formula, data = data$trainSet, distribution = "gaussian", 
                 bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)
  
  summary(gbm.orch)
  gbm.boost = predict(gbm.orch , newdata=data$testSet, n.trees=trees)
  
  testError = testError + evaluateModel(gbm.boost, data$testAnswers, useLogTransform)
  
}

tries = length(seeds)
print(paste("Final test error=", testError / tries, " based on ", tries, " tries"))
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

print(data$allSetAll["306",])
print(data$trainAccountsId[1487,1])
dim(data$trainAccountsId)


gbm.orch = gbm(formula, data=data$allSet,distribution="gaussian", 
               bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)

summary(gbm.orch)

predictSet = prepareDataToPredict(data$predictors)
predictions = predict(gbm.orch, newdata=predictSet$testSet, n.trees=trees)

if(useLogTransform) {
  predictions = exp(predictions)-1
}

dumpResponse("MS_gbm_sub", predictSet$accounts, predictions)
