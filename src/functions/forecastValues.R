source("../functions/predictLMEquation.R")
source("../functions/nameDateColumns.R")

forecastValues <- function(initVals, lmList, varsListLM, 
                           regPowersListLM, resPowersListLM, offsetListLM,
                           latVals, longVals, days = 3){
  
  #Transforming Regression model into (days) long forecast
  
  #Dimensionality and Length Checks
  stopifnot(is.data.frame(initVals) | is.list(lmList) |
              length(lmList) == length(varsListLM) |
              length(varsListLM) == length(regPowersListLM) |
              length(regPowersListLM) == length(resPowersListLM) |
              length(resPowersListLM) == length(offsetListLM) |
              length(lmList) + 2 == max(dim(initVals)[1],dim(initVals)[2]) |
              length(latVals) == length(longVals) |
              length(latVals) == days)
  
  #Initialize our Forecast & Temporary Column:
  forecastPrediction <- initVals
  nrowcount <- max(dim(initVals)[1],dim(initVals)[2])
  
  #Estimating our responses:
  
  #Iterating over days:
  for(i in 1:days) {
    dayPrediction <- as.data.frame(matrix(NA, nrow = nrowcount, ncol = 1))
    dayPrediction[1,1] <- latVals[i]
    dayPrediction[2,1] <- longVals[i]
    
    #Iterating over models (To Grab Coefficients):
    for(j in 1:length(lmList)){
      curCoefs <- as.numeric(lmList[[j]][["coefficients"]])
      #Is List (For Interaction Coefficients)
      if(is.list(varsListLM[[j]])){
        curVars <- as.numeric(matrix(0, nrow = length(varsListLM[[j]]), ncol = 1))
        for(k in 1:length(varsListLM[[j]])){
          curVars[k] <- prod(forecastPrediction[varsListLM[[j]][[k]],i])
        }
      }
      
      #Non-Interaction Coefficients
      else{
        curVars <- forecastPrediction[varsListLM[[j]],i]
      }
      curRegPowers <- regPowersListLM[[j]]
      curResPower <- resPowersListLM[[j]]
      curOffset <- offsetListLM[[j]]
      
      #Save Predicted Values
      dayPrediction[j+2,1] <- 
        predictLMEquation(curCoefs, curVars, curRegPowers, curResPower, curOffset)
    }
    #Update Forecast
    forecastPrediction <- cbind(forecastPrediction, dayPrediction)
  }
  
  #Return Forecast DF:
  colnames(forecastPrediction) <- nameDateColumns(forecastPrediction)
  return(forecastPrediction)
}