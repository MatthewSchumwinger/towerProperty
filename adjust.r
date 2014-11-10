

validationRatio = 0.15
filter = "199|price.level|add_no|TELEMAN|JOHANN|ROSSINI|conc_missed|add_price|add_tickets|add_tickets_seats|section_2013_2014|multiple.subs|billing.city|is.us|relationship|outside|City|State|Lat|Long" 
rawData = readData(useLogTransform)
predictors = preparePredictors(rawData, filter, validationRatio)
data = prepareSplits(rawData, predictors)

predictSet = prepareDataToPredict(data$predictors)

predictions = read.csv('ML_gbm_sub_adjNov_05_2014.csv',colClasses='character')
predictions = sapply(predictions$total, as.numeric)

# light variant
activeClients = predictSet$testSet$total_2010_2011 + predictSet$testSet$total_2011_2012 + predictSet$testSet$total_2012_2013 + predictSet$testSet$total_2013_2014
activeClientsBinary = sapply(activeClients > 0, as.numeric)
predictions = predictions * activeClientsBinary

hard=TRUE
# hard variant
if(hard) {
  df = data.frame(predictSet$testSet$total_2009_2010, predictSet$testSet$total_2010_2011, predictSet$testSet$total_2011_2012, predictSet$testSet$total_2012_2013, predictSet$testSet$total_2013_2014)
  varClients = apply(df, 1, var)
  
  varNonZeroClients = sapply(varClients != 0, as.numeric)
  varZeroClients = sapply(varClients == 0, as.numeric)
  
  newPredictions = varZeroClients * predictSet$testSet$total_2013_2014
  predictions = predictions * varNonZeroClients + newPredictions
}
# end hard variant

dumpResponse("ML_gbm_sub_adj3", predictSet$accounts, predictions)
