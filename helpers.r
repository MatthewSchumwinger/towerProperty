
adjustAnswer = function(score) {
  
  if(score < 0.0) {
    return (0)
  }
  return (score)
  #answerIncreaseFactor=0.05
  #return (round(score*2 + answerIncreaseFactor)/2)
}

prepareFormula = function() {
  polyOrder = 1
  
  formula = as.formula(
    total ~ . 
    #+
    # polynomials for total subs in last 3 years
    #poly(total_2011_2012, polyOrder) + 
    #poly(total_2012_2013, polyOrder) + 
    #poly(total_2013_2014, polyOrder) +
    
    # log polynomials for total subs in last 3 years
    #poly(log(1+total_2011_2012), polyOrder) +
    #poly(log(1+total_2012_2013), polyOrder) +
    #poly(log(1+total_2013_2014), polyOrder) 
    
    # intercations between price levels and total_subscriptions
    #poly(price.level_2011_2012*total_2011_2012, polyOrder) + 
    #poly(price.level_2012_2013*total_2012_2013, polyOrder) + 
    #poly(price.level_2013_2014*total_2013_2014, polyOrder) +
    
    #poly(log(1+price.level_2012_2013*total_2012_2013), polyOrder) + 
    #poly(log(1+price.level_2013_2014*total_2013_2014), polyOrder)
    
    # intercations between no.seats and price_levels
    #poly(price.level_2011_2012*no.seats_2011_2012, polyOrder) + 
    #poly(price.level_2012_2013*no.seats_2012_2013, polyOrder) + 
    #poly(price.level_2013_2014*no.seats_2013_2014, polyOrder)
  )
  
  return (formula)
}


evaluateModel = function(predictions, correctAnswers) {
  
  # compute validatation test error
  
  answers = c()
  error = 0;
  for(i in 1:length(correctAnswers)) {
    correctAnswer = correctAnswers[[i]]
    answer = adjustAnswer(predictions[i])
    answers = c(answers, answer)
    error = error + (log(answer+1)-log(correctAnswer+1))^2
  }
  
  error = error / length(correctAnswers)
  error = sqrt(error)
  print(paste("Error in validation set: ", error, " based on: ", length(correctAnswers), " samples"))
  return (error)
}
