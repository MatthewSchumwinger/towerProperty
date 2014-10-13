

trainLDAModel = function(formula, data) {
  
  lda.fit = lda(formula , data = data)
  return (lda.fit)
}

predictFromLDAModel = function(fit, set) {
  lda.predTest = predict(fit, set)
  characters = sapply(lda.predTest$class, as.character)
  numerics = sapply(characters, as.numeric)
  return (numerics)
}

debugLDAFit = function(predictions, correctAnswers) {
  table(predictions, correctAnswers)
}

asFactor = TRUE
trainModel = trainLDAModel
predictFromModel = predictFromLDAModel

debugFit = function(fit){ return (c()) }
debugFitOnValidationSet = debugLDAFit
