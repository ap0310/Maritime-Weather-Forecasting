nameDateColumns <- function(dataframe){
  #Function only "works" for 29 days (Naming)
  nameList <- rep(NA, dim(dataframe)[2])
  for(i in 1:dim(dataframe)[2]){
    if(i == 1){
      nameList[i] <- "Jan 31"
    }
    else{
      nameList[i] <- paste("Feb", as.character(i-1))
    }
  }
  return(nameList)
}