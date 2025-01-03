predictLMEquation <- function(coefs, vals, regressPowers, responsePower = 1, responseOffSet = 0){
  
  #Returns predicted value based on a given lm's values
  
  #Argument Checks
  stopifnot(length(vals) == length(regressPowers) |
              length(vals) + 1 == length(coefs) |
              length(regressPowers) + 1 == length(coefs))
  predictedValue <- coefs[1]
  
  #Calculate Response
  for (i in 1:length(vals)) {
    if(regressPowers[i] == 0){ #log
      predictedValue <- predictedValue + (coefs[i+1] * log(vals[i]))
    }
    else{
      predictedValue <- predictedValue + (coefs[i+1] * (vals[i])^regressPowers[i])
    }
  }
  #Normalize Response Value
  if(responsePower == 0){ #log
    return(exp(predictedValue) - responseOffSet)
  }
  else{
    return((predictedValue^(responsePower^-1)) - responseOffSet)
  }
}