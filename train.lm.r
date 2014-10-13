
trainLMModel = function(formula, data) {
  
  lm.fit = lm(formula, data=data[,])
  return (lm.fit)
}

debugLMFit = function(fit) {
  summary(fit)
  par(mfrow=c(2,2))
  plot(fit)
}


predictFromLMModel = function(fit, set) {
  pr = predict(fit, newdata=set, type="response")
  return (pr)
}
  

debugFitAndResiduals = function(predictions, correctAnswers) {

  residuals = c()
  answers = c()
  for(i in 1:length(correctAnswers)) {
    correctAnswer = correctAnswers[[i]]
    answer = adjustAnswer(predictions[i])
    residuals = c(residuals, correctAnswer-answer)
    answers = c(answers, answer)
  }

  par(mfrow=c(1,1))
  plot(correctAnswers, residuals)
  plot(answers, residuals)
  plot(correctAnswers, answers)
}

