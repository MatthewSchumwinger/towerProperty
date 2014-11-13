
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

useLogTransform = FALSE 
numfolds = 10  

rawData = readData(useLogTransform)

formula = prepareFormula(useLogTransform)

set.seed(551724)
folds = sample(1:numfolds, nrow(allData$allSet), replace=T)

while(TRUE) {

  set.seed(as.numeric(Sys.time()))
  tokens = c("199", "200", "2000|2001|2002|2003|2004", "2005|2006|2007|2008", "2009", "2010", "2011", "2012", "2013",
             "price.level", "add_no", "TELEMAN|JOHANN|ROSSINI",  "HANDEL", "VIVALDI", "HAYDN", "BACH",
             "conc_missed", "add_price", "add_tickets", "add_tickets_seats", "multiple.subs", "is.us", 
             "relationship", "outside", "City", "State", "Lat|Long", 
             "2.years", "3.years", "4.years|5.years", "6.years|7.years|8.years",
             "donating", "package", "no.seats", "location", "total")
  
  setTrees = c(500, 1000, 1500, 2000, 2500, 3000, 4000, 2000, 2500, 3000, 4000, 5000, 6000, 8000, 10000, 15000)
  setShrinkage = c(0.001, 0.001, 0.001, 0.001, 0.0005, 0.002, 0.005, 0.01)
  setBagfrac = c(0.5, 0.5, 0.5, 0.66, 0.4, 0.6)
  setDepths = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7, 8, 9, 10)
  
  start_filter = "billing.city|section_2013_2014"

  numTokens = sample(1:length(tokens), 1)  
  selected = sample(1:length(tokens), numTokens, replace=FALSE)

  filter = paste(c(start_filter, paste(tokens[selected], collapse="|")), collapse="|")
  
  trees = setTrees[sample(1:length(setTrees), 1)]
  shrinkage = setShrinkage[sample(1:length(setShrinkage), 1)]
  bagfrac = setBagfrac[sample(1:length(setBagfrac), 1)]
  depth = setDepths[sample(1:length(setDepths), 1)]

  while((trees > 5000) && (depth > 5) && (numTokens < 26))
  {
    trees = setTrees[sample(1:length(setTrees), 1)]
    depth = setDepths[sample(1:length(setDepths), 1)]    
  }
  
  print(paste("filter=", filter))
  print(paste("trees=", trees))
  print(paste("bagfrac=", bagfrac))
  print(paste("depth=", depth))
  
  predictors = preparePredictors(rawData, filter)
  
  testError = 0
  testErrorInact = 0
  testErrorVar = 0
  for(i in 1:numfolds) {
    
    print(paste("Starting fold", i))
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
  
  tries = numfolds
  print(paste("Final test error raw prediction=", testError / tries, " based on ", tries, " tries"))
  print(paste("Final test error with inactive adj=", testErrorInact / tries, " based on ", tries, " tries"))
  print(paste("Final test error with no variance adj =", testErrorVar / tries, " based on ", tries, " tries"))
  
  data=cbind(filter,trees,bagfrac,shrinkage,depth)
  threshold = 0.0925
  
  if((testError/tries) < threshold) {
    write.csv(data, paste(testError, "_raw_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = F)  
  }
  
  if((testErrorInact/tries) < threshold) {
    write.csv(data, paste(testError, "_inact_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = F)  
  }
  
  if((testErrorVar/tries) < threshold) {
    write.csv(data, paste(testError, "_invar_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""), row.names = F)  
  }
}

summary(gbm.orch)

#deep check
#for(i in 1:length(data$testAnswers)) {
#  correctAnswer = data$testAnswers[[i]]
#  answer = adjustAnswer(gbm.boost[i])
#  if(abs(answer-correctAnswer) > 1) {
#    print(paste("Big error for ", i, " predicted=", answer, " correct=", correctAnswer))
#    print(data$testSet[i, grep("total_201", colnames(data$testSet))])
#  } 
#}  

#print(data$allSetAll["5905",])
#print(data$trainAccountsId[1487,1])
#dim(data$trainAccountsId)


gbm.orch = gbm(formula, data=data$allSet,distribution="gaussian", 
               bag.fraction = bagfrac, shrinkage = shrinkage, n.trees = trees, interaction.depth = depth)

summary(gbm.orch)

predictSet = prepareDataToPredict(data$predictors)
predictSetAll = prepareDataToPredict(allData$predictors)
predictions = predict(gbm.orch, newdata=predictSet$testSet, n.trees=trees)

predictions = adjustPredictionsInactive(predictions, data.frame("account.id"=predictSet$accounts), 
                                        predictSetAll$testSetAll)

dumpResponse("ML_gbm_sub", predictSet$accounts, predictions)
