
adjustAnswer = function(score) {
  
  score = as.numeric(score)
  threshold = 0.03
  if(score < threshold) {
    return (0)
  }
  candidate = round(score*2)/2
  if(abs(candidate - score) < threshold) {
    return (candidate)
  }
  return (score)
}

prepareFormula = function(useLogTransform) {
  polyOrder = 2
  
  # we might try different formulas for log transform and without it
  if(useLogTransform) {
    
    #formula for log transformation
    formula = as.formula(
      total ~ . # +
      # log polynomials for total subs
      #poly(log(1+total_2008_2009), polyOrder) + 
      #poly(log(1+total_2009_2010), polyOrder) + 
      #poly(log(1+total_2010_2011), polyOrder) + 
      #poly(log(1+total_2011_2012), polyOrder) + 
      #poly(log(1+total_2012_2013), polyOrder) + 
      #poly(log(1+total_2013_2014), polyOrder) +
      
      # log polynomials for no seats 
      #poly(log(1+no.seats_2008_2009), polyOrder) + 
      #poly(log(1+no.seats_2009_2010), polyOrder) + 
      #poly(log(1+no.seats_2010_2011), polyOrder) +
      #poly(log(1+no.seats_2011_2012), polyOrder) +
      #poly(log(1+no.seats_2012_2013), polyOrder) +
      #poly(log(1+no.seats_2013_2014), polyOrder) +
      
      # log polynomials for prive levels
      #poly(log(1+price.level_2008_2009), polyOrder) + 
      #poly(log(1+price.level_2009_2010), polyOrder) + 
      #poly(log(1+price.level_2010_2011), polyOrder) +
      #poly(log(1+price.level_2011_2012), polyOrder) +
      #poly(log(1+price.level_2012_2013), polyOrder) +
      #poly(log(1+price.level_2013_2014), polyOrder) 
    )
    
  } else {
    
    # regular case
    
    formula = as.formula(
      total ~ . 
      #+
      # polynomials for total subs in last 3 years
      #poly(log(1+total_2010_2011), polyOrder) + 
      #poly(log(1+total_2011_2012), polyOrder) + 
      #poly(log(1+total_2012_2013), polyOrder) + 
      #poly(log(1+total_2013_2014), polyOrder) +
      
      # log polynomials for total subs in last 3 years
      #poly(log(1+no.seats_2010_2011), polyOrder) +
      #poly(log(1+no.seats_2011_2012), polyOrder) +
      #poly(log(1+no.seats_2012_2013), polyOrder) +
      #poly(log(1+no.seats_2013_2014), polyOrder) +
      
      #poly(log(1+price.level_2010_2011), polyOrder) +
      #poly(log(1+price.level_2011_2012), polyOrder) +
      #poly(log(1+price.level_2012_2013), polyOrder) +
      #poly(log(1+price.level_2013_2014), polyOrder) 
      
      # intercations between price levels and total_subscriptions
      #poly(price.level_2011_2012*total_2011_2012, polyOrder) + 
      #poly(price.level_2012_2013*total_2012_2013, polyOrder) + 
      #poly(price.level_2013_2014*total_2013_2014, polyOrder) +
      #poly(log(1+no.seats_2010_2011*total_2010_2011), polyOrder) + 
      #poly(log(1+no.seats_2011_2012*total_2011_2012), polyOrder) + 
      #poly(log(1+no.seats_2012_2013*total_2012_2013), polyOrder) + 
      #poly(log(1+no.seats_2013_2014*total_2013_2014), polyOrder) +    
      
      #poly(log(1+price.level_2010_2011*total_2010_2011), polyOrder) + 
      #poly(log(1+price.level_2011_2012*total_2011_2012), polyOrder) + 
      #poly(log(1+price.level_2012_2013*total_2012_2013), polyOrder) + 
      #poly(log(1+price.level_2013_2014*total_2013_2014), polyOrder)
      
      # intercations between no.seats and price_levels
      #poly(price.level_2011_2012*no.seats_2011_2012, polyOrder) + 
      #poly(price.level_2012_2013*no.seats_2012_2013, polyOrder) + 
      #poly(price.level_2013_2014*no.seats_2013_2014, polyOrder)
    )
    
    
  }

  return (formula)
}

adjustPredictionsRound = function(myPredictions, accounts, allTestSet) {
  return (round(myPredictions*2)/2)
}

adjustPredictionsInactive = function(myPredictions, accounts, allTestSet) {

  filteredMySet = merge(accounts, allTestSet, by="account.id", all.x=T, all.y=F)

  activeClients = filteredMySet$total_2008_2009 + filteredMySet$total_2009_2010 + filteredMySet$total_2010_2011 + filteredMySet$total_2011_2012 + filteredMySet$total_2012_2013 + filteredMySet$total_2013_2014
  activeClientsBinary = sapply(activeClients > 0, as.numeric)
  
  myPredictions = myPredictions * activeClientsBinary
  return(myPredictions)
  
}

adjustPredictionsGeo = function(myPredictions, accounts, allTestSet) {
  
  filteredMySet = merge(accounts, allTestSet, by="account.id", all.x=T, all.y=F)
  
  caClients = as.character(filteredMySet$State) == "CA" 
  caClients[is.na(caClients)] <- F 
  caClientsBinary = sapply(caClients > 0, as.numeric)
  
  myPredictions = myPredictions * caClientsBinary
  return(myPredictions)
  
}

adjustPredictionsGeoDist = function(myPredictions, accounts, allTestSet) {
  
  filteredMySet = merge(accounts, allTestSet, by="account.id", all.x=T, all.y=F)
  
  distClients = as.numeric(filteredMySet$dSF) 
  distClients[is.na(distClients)] <- 0 
  
  distClientsClose = sapply(distClients <= 300, as.numeric)
  distClientsClose[distClientsClose == 1] = 10000
  distClientsClose[distClientsClose == 0] = 0
  
  distClientsVeryClose = sapply(distClients <= 60, as.numeric)
  distClientsVeryClose[distClientsVeryClose == 1] = 10000
  distClientsVeryClose[distClientsVeryClose == 0] = 1
  
  myPredictions = pmin(myPredictions, distClientsClose)
  myPredictions = pmin(myPredictions, distClientsVeryClose)  
  return(myPredictions)
  
}

adjustPredictionsInvariant = function(myPredictions, accounts, allTestSet) {

  myPredictions = adjustPredictionsInactive(myPredictions, accounts, allTestSet)
  
  filteredMySet = merge(accounts, allTestSet, by="account.id", all.x=T, all.y=F)
  df = data.frame(filteredMySet$total_2005_2006, filteredMySet$total_2006_2007, filteredMySet$total_2007_2008, filteredMySet$total_2008_2009, filteredMySet$total_2009_2010, filteredMySet$total_2010_2011, filteredMySet$total_2011_2012, filteredMySet$total_2012_2013, filteredMySet$total_2013_2014)
  varClients = apply(df, 1, var)
  
  varNonZeroClients = sapply(varClients != 0, as.numeric)
  varZeroClients = sapply(varClients == 0, as.numeric)
  
  newPredictions = varZeroClients * filteredMySet$total_2013_2014
  newPredictions = myPredictions * varNonZeroClients + newPredictions
  return(newPredictions)
  
}

evaluateModel = function(predictions, correctAnswers, useLogTransform) {
  
  # compute validatation test error
  
  answers = c()
  error = 0;
  for(i in 1:length(correctAnswers)) {
    correctAnswer = correctAnswers[[i]]
    answer = adjustAnswer(predictions[i])
    answers = c(answers, answer)
    if(useLogTransform) {
      error = error + (answer-correctAnswer)^2
    } else {  
      error = error + (log(answer+1)-log(correctAnswer+1))^2 
    } 
  }
  
  error = error / length(correctAnswers)
  error = sqrt(error)
  print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))
  return (error)
}

normalizeStrings = function (data) {
  tmp = iconv(data,"WINDOWS-1252","UTF-8")
  tmp = sapply(tmp, tolower)
  tmp = sapply(tmp, str_trim)
  return (tmp)
}

# special helper function for Matt's environment

setConfigForMac = function () {
  ## text files needing path changes
  filenames <- c( "data.r", "msTest.r" ) # msTest.r is a dummy file place holder
  
  ## change path from PC "\\" to Mac "/"
  for( f in filenames ){
    path.x <- readLines(f)
    path.y <- gsub( "\\\\\\\\", "/", path.x )
    cat(path.y, file=f, sep="\n")
  }
  
  ## Review output
  for( f in filenames ){ 
    print(head(readLines(f)))
  }
}

setConfigForMyEnvironment = function () {
  setConfigForMac()
  #setConfigForWindows() 
}
