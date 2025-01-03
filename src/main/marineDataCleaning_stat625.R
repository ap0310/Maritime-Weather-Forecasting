# This .R will contain the analysis on the Marine measurements dataset
# as well as Figures for the LaTeX Project Report.
# Data: https://www.ncei.noaa.gov/data/global-marine/doc/
# Documentation: https://www.ncei.noaa.gov/data/global-marine/doc/marinedoc.pdf

#Dataset:
marineData <- read.csv("../../volume/data/Marine_CSV_sample.csv") #Raw Data

#CLEANING:

#Removing Useless Columns:
marineData <- marineData[,colSums(is.na(marineData))!=nrow(marineData)] #Null Cols
marineData <- marineData[,-c(1,7)] #IDs/Redundant Data (Same location)

#Latitude and Longitude (Degrees From 0,0) Unchanged

#Since measurements are over (1/12/15 to 1/31/15) and occur at hours
#0, 6, 12, 18, we can split Time.Of.Observation into 2 variables:
#Hour.of.Observation (4 Level Factor) and 
#Day.of.Observation (Numeric, only January 2015).

#Splitting Time of Observation:

#Hour of Observation (xx:00) (Simplify to 4 Levels):
marineData$Hour.of.Observation <- substring(marineData$Time.of.Observation, 12,13)
marineData$Hour.of.Observation[(marineData$Hour.of.Observation == 13)] <- 12 #1 Obs
marineData$Hour.of.Observation <- as.factor(marineData$Hour.of.Observation)

#Day of Observation (January):
marineData$Day.of.Observation <- substring(marineData$Time.of.Observation, 9,10)
marineData$Day.of.Observation <- as.numeric(marineData$Day.of.Observation)

#Remove Time of Observation column, it has no useful information remaining:
marineData <- marineData[,-3]

#Sea Level Pressure (Millibars):
marineData$Sea.Level.Pressure <- marineData$Sea.Level.Pressure * 33.8639

#Characteristics of Pressure (Simplify to 2 Levels):
marineData$Characteristics.of.Pressure.Tendency <- as.character(marineData$Characteristics.of.Pressure.Tendency)
highPresVals <- c("0", "1", "2", "3") #Case 0: P(Same) = 0 (Continuous RV)
lowPresVals <- c("5", "6", "7", "8") #Case 5: P(Same) = 0 (Continuous RV)

for (i in 1:4) {
  marineData$Characteristics.of.Pressure.Tendency[marineData$Characteristics.of.Pressure.Tendency == highPresVals[i]] <-
    "Increasing Pressure"
}

for (i in 1:4) {
  marineData$Characteristics.of.Pressure.Tendency[marineData$Characteristics.of.Pressure.Tendency == lowPresVals[i]] <-
    "Decreasing Pressure"
}
marineData$Characteristics.of.Pressure.Tendency <- as.factor(marineData$Characteristics.of.Pressure.Tendency)

#Air Temperature (Celsius):
marineData$Air.Temperature <- round((marineData$Air.Temperature - 32) * (5/9),2)

#Dew Point Temperature (Celsius):
marineData$Dew.Point.Temperature <- round((marineData$Dew.Point.Temperature - 32) * (5/9),2)

#Wave Period (Avg. Seconds Between Period):
waveCatValues <- c(2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 99)
changeVals <- c(4.9, 6.5, 8.5, 10.5, 12.5, 14.5, 16.5, 18.5, 20.5, 22, NA)
for (i in 1:11) {
  marineData$Wave.Period[marineData$Wave.Period == waveCatValues[i]] <- changeVals[i]
}

#Wave Height (Meters):
marineData$Wave.Height <- marineData$Wave.Height * 0.5

#Swell Direction (Degrees Clockwise from N):
marineData$Swell.Direction[marineData$Swell.Direction == 38] <- NA #Meaningless
marineData$Swell.Direction <- marineData$Swell.Direction*10

#Swell Period (Seconds) Unchanged

#Swell Height (Meters):
marineData$Swell.Height <- marineData$Swell.Height * 0.5

#Total Cloud Amount (Proportion):
marineData$Total.Cloud.Amount <- marineData$Total.Cloud.Amount * 0.125

#Low Cloud Amount (Proportion):
marineData$Low.Cloud.Amount <- marineData$Low.Cloud.Amount * 0.125

#Cloud Height (Meters):
marineData$Cloud.Height[marineData$Cloud.Height == "9"] <- NA #Unmeasurable
marineData$Cloud.Height[marineData$Cloud.Height == "A"] <- NA #Unmeasurable
marineData$Cloud.Height <- as.numeric(marineData$Cloud.Height)
cheightVals <- c(2, 3, 4, 5, 6)
replaceVals <- c(150, 250, 450, 800, 1250)
for (i in 1:5){
  marineData$Cloud.Height[marineData$Cloud.Height == cheightVals[i]] <- replaceVals[i]
}

#Middle Cloud Type (Simplify to 3 Levels):
marineData$Middle.Cloud.Type[marineData$Middle.Cloud.Type == "A"] <- NA #No Class
marineData$Middle.Cloud.Type[marineData$Middle.Cloud.Type == " "] <- NA #No Class
marineData$Middle.Cloud.Type[marineData$Middle.Cloud.Type == "0"] <- "No Middle Clouds"
alOrNimVals <- c("1", "2")
altoVals <- c("3", "6", "7", "8")

for (i in 1:2){
  marineData$Middle.Cloud.Type[marineData$Middle.Cloud.Type == alOrNimVals[i]] <- 
    "Altostratus or Nimbostratus"
}

for (i in 1:4){
  marineData$Middle.Cloud.Type[marineData$Middle.Cloud.Type == altoVals[i]] <- 
    "Altocumulus"
}
marineData$Middle.Cloud.Type <- as.factor(marineData$Middle.Cloud.Type)

#High Cloud Type (Simplify to 3 Levels):
marineData$High.Cloud.Type[marineData$High.Cloud.Type == "A"] <- NA #No Class
marineData$High.Cloud.Type[marineData$High.Cloud.Type == " "] <- NA #No Class
marineData$High.Cloud.Type[marineData$High.Cloud.Type == "0"] <- "No High Clouds"
cirrusVals <- c("1", "4")
cirrostratusVals <- c("5", "8")

for (i in 1:2){
  marineData$High.Cloud.Type[marineData$High.Cloud.Type == cirrusVals[i]] <- 
    "Cirrus"
}

for (i in 1:2){
  marineData$High.Cloud.Type[marineData$High.Cloud.Type == cirrostratusVals[i]] <- 
    "Cirrostratus+"
}
marineData$High.Cloud.Type <- as.factor(marineData$High.Cloud.Type)

#Visibility (Kilometers):
visVals <- c(97, 98)
repVals <- c(10, 20)

for(i in 1:2){
  marineData$Visibility[marineData$Visibility == visVals[i]] <- repVals[i]
}

#Present Weather (Simplify to 3 Levels):
marineData$Present.Weather <- as.character(marineData$Present.Weather)
marineData$Present.Weather[marineData$Present.Weather == "80"] <- "Light Precipitation"
noPrecipVals <- c("1", "2", "3", "13") #WMO4677 Code 13 "fits" best here.
mod2heavyPrecipVals <- c("62", "82", "90", "92")

for (i in 1:4){
  marineData$Present.Weather[marineData$Present.Weather == noPrecipVals[i]] <- 
    "No Precipitation"
}

for (i in 1:4){
  marineData$Present.Weather[marineData$Present.Weather == mod2heavyPrecipVals[i]] <- 
    "Moderate to Heavy Precipitation"
}
marineData$Present.Weather <- as.factor(marineData$Present.Weather)

#Past Weather (Simplify to 3 Levels):
marineData$Past.Weather <- as.character(marineData$Past.Weather)
marineData$Past.Weather[marineData$Past.Weather == "0"] <- "Cloud Cover 0.5 or Less"
marineData$Past.Weather[marineData$Past.Weather == "1"] <- "Mixed Cloud Cover"
moreCloudVals <- c("2", "9")

for(i in 1:2){
  marineData$Past.Weather[marineData$Past.Weather == moreCloudVals[i]] <-
    "Cloud Cover More Than 0.5"
}
marineData$Past.Weather <- as.factor(marineData$Past.Weather)

#Wind Direction (Degrees Clockwise from N) Unchanged

#Wind Speed (Meters Per Second):
marineData$Wind.Speed <- marineData$Wind.Speed/10

#EXPORT:
write.csv(marineData, "../../volume/data/cleanedMarineData.csv", row.names = FALSE)