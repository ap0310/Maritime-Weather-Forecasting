#We will perform analysis on the Cleaned Marine dataset.
#To self-generate the cleaned & transformed data, go to
#marineDataCleaning_stat625.R

#There are 5 Main Sections:
#A: Analyzing and Visualization (EDA)
#B: Fitting Linear Models
#C: Using (B) to Create Weather Forecast.
#D: Visualizing Output and Paths of (C)
#E: Miscellaneous Figures

#Import Data:
marine <- read.csv("../../volume/data/cleanedMarineData.csv")

#Import Functions:
source("../functions/IR_BC_plot.R")
source("../functions/predictLMEquation.R")
source("../functions/forecastValues.R")
source("../functions/nameDateColumns.R")


#A: ANALYSIS & VISUALIZATION:


library(ggplot2)
library(maps)
library(MASS)
library(car)

#Ship Path (Jan 12th to Jan 31st):

png("../../volume/figures/Buoy_Path_D5GN6.png",
    width = 600,
    height = 360)

worldMap <- map_data("world")
ggplot() +
  geom_map(data = worldMap, 
           map = worldMap,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgreen") + 
  geom_line(data = marine,
             aes(Longitude, Latitude),
             color = "red",
            lwd = 1) + 
  annotate("text", label = " 12th", color = "black",
           x = 3, y = -39, size = 3) + 
  annotate("text", label = " 31st", color = "black",
           x = 90, y = 9, size = 3) + 
  labs(title = "D5GN6 Buoy Path, January 2015")

dev.off()

#Categorical Data Analysis (How to make our forecast):

#Scatterplot Matrix of Predictors:

png("../../volume/figures/forecastVarsUnfix.png",
    width = 1200,
    height = 720)

scatterplotMatrix(~ Air.Temperature + Total.Cloud.Amount + 
        Wind.Speed + Wind.Direction + Sea.Level.Pressure + 
        Wave.Height + Swell.Height, data = marine)

dev.off()

#Outlier Cleaning:

marine$Wave.Height[marine$Wave.Height > 10] <- NA #Omit 1 outlier (31.5m)
marine$Wind.Speed[marine$Wind.Speed > 25] <- NA #Omit 1 outlier (46.2m/s)

png("../../volume/figures/forecastVarsOutRemoved.png",
    width = 1200,
    height = 720)

scatterplotMatrix(~ Air.Temperature + Total.Cloud.Amount + 
                    Wind.Speed + Wind.Direction + Sea.Level.Pressure + 
                    Wave.Height + Swell.Height, data = marine)

dev.off()

#Creating Numeric Reference of Precipitation Class:

nameVals <- c("No Precipitation", "Light Precipitation", "Moderate to Heavy Precipitation")
numVals <- c(0.01, 0.51, 1.01)

for(i in 1:3){
  marine$Present.Weather.Numeric[marine$Present.Weather == nameVals[i]] <- 
    numVals[i]
}


#B: LINEAR MODELS (8 Models):


#Comments and number separate each model for later.
#Regressor comments explain reasoning behind each regressor.
#Additional notes will be added when necessary.

#NOTE: I fitted these models to be more dependent on location
#Rather than day as coordinates varied much more than day over the data.


#1. Temperature (Degrees Celsius):


#Latitude^2: In relation to Equator
#Wind Direction (Location Specificity, Likely Unstable).
#Sea Level Pressure: Higher pressure = Colder Air (Density).
#Total Cloud Amount^2: Clouds cover sun 2-Dimensionally.

#Linear Model:
temperatureModel <- lm(Air.Temperature ~ 
                         I(Latitude^2) + Sea.Level.Pressure + 
                          I(Total.Cloud.Amount^2) + Wind.Direction, data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(temperatureModel) #No

#Coefficients in Model
summary(temperatureModel)

#References for Forecast Model:
temperatureVars <- c(1,3,7,8)
temperatureRegPowers <- c(2,1,2,1)
temperatureResPowers <- 1
temperatureOffset <- 0


#2. Cloud Coverage (% or oktas/8):


#Air Temperature: Cold air associated with storms
#Wind Direction: Tropical Easterlies

cloudModel <- lm(Total.Cloud.Amount ~ Air.Temperature + Wind.Direction, 
                 data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(cloudModel) #No

#Coefficients in Model
summary(cloudModel)

#References for Forecast Model:
cloudVars <- c(4,8)
cloudRegPowers <- c(1,1)
cloudResPowers <- 1
cloudOffset <- 0


#3. Precipitation (Categorical Likelihood):


#Latitude^2: Equator has warmer, more humid air.
#Wind Speed and Direction: Transport air to create condensation.

#NOTE: Sea Level Pressure should be included, but it is not statistically
#significant in our model.

precipModel <- lm(Present.Weather.Numeric ~ I(Latitude^2) + 
                    Wind.Direction + Wind.Speed, data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(precipModel) #No: (This is a Categorical Variable, Meaningless)

#Coefficients in Model
summary(precipModel)

#References for Forecast Model:
precipVars <- c(1,8,9)
precipRegPowers <- c(2,1,1)
precipResPowers <- 1
precipOffset <- 0


#4. Wind Speed:


#Latitude^2: Winds get stronger from equator.
#Wind Direction: Tropical Easterlies.
#Air Temperature-Sea Level Pressure Interaction: 
#Warmer air is correlated to lower air pressure. As such, these factors
#Interact directly in nature and warrant interaction here.

#Note: Swell Height is affected by Wind Speed, NOT the other way around.
#As such, Swell Height is colinear with Wind Speed, drop.

windSpeedModel <- lm(Wind.Speed ~ I(Latitude^2) + Sea.Level.Pressure + 
                       Air.Temperature + Wind.Direction + 
                       Sea.Level.Pressure:Air.Temperature, 
                     data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(windSpeedModel) #No

#Coefficients in Model
summary(windSpeedModel)

#References for Forecast Model:
windSpeedVars <- list(1,3,4,8,c(3,4))
windSpeedRegPowers <- c(2,1,1,1,1)
windSpeedResPowers <- 1
windSpeedOffset <- 0


#5. Wind Direction:


#Latitude^2: Winds affected by Tropical Easterlies.
#Air Temperature: Naive way of using Air pressure.

windDirModel <- lm(I(Wind.Direction^0.5) ~ I(Latitude^2) + Air.Temperature, 
                   data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(windDirModel) #Yes. (sqrt), (RSS(1) ~ RSS(1.7))

#Coefficients in Model
summary(windDirModel)

#References for Forecast Model:
windDirVars <- c(1,4)
windDirRegPowers <- c(2,1)
windDirResPowers <- 0.5
windDirOffset <- 0


#6. Sea Level Pressure:


#Air Temperature: Warmer Air = Lower Pressure (Physics).
#Wind Speed: Wind Speed has air flow.
#Wind Speed^2: Applying in 2 Dimensions.

pressureModel <- lm(Sea.Level.Pressure ~ Air.Temperature + Wind.Speed + 
                      I(Wind.Speed^2), data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(pressureModel) #No (Meaningless)

#Coefficients in Model
summary(pressureModel)

#References for Forecast Model:
pressureVars <- c(4,9,9)
pressureRegPowers <- c(1,1,2)
pressureResPowers <- 1
pressureOffset <- 0


#7. Wave Height:


#Latitude^2: Winds get stronger toward the poles.
#Wind Speed and Wind Direction: Major factors of creating waves.

waveHeightModel <- lm(I((Wave.Height + .01)^0.5) ~ I(Latitude^2) + 
                         Wind.Direction + Wind.Speed, data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(waveHeightModel) #Yes, (sqrt)

#Coefficients in Model
summary(waveHeightModel)

#References for Forecast Model:
waveHeightVars <- c(1,8,9)
waveHeightRegPowers <- c(2,1,1)
waveHeightResPowers <- 0.5
waveHeightOffset <- 0.01


#8. Swell Height:


#Latitude^2: Winds get stronger toward the poles.
#Wind Speed: Major factor of creating swells.
#Wind Speed^2: Swells deal with length, are more 2-D than waves.

swellHeightModel <- lm(I(Swell.Height^2) ~ I(Latitude^2) + 
                         Wind.Speed + I(Wind.Speed^2), data = marine)

#Transformation needed?
par(mfrow = c(1,2))
IR_BC_plot(swellHeightModel) #Yes. (Squared)

#Coefficients in Model
summary(swellHeightModel)

#References for Forecast Model:
swellHeightVars <- c(1,9,9)
swellHeightRegPowers <- c(2,1,2)
swellHeightResPowers <- 2
swellHeightOffset <- 0

#Creating BoxCox-InverseResponse Plot Matrix:

png("../../volume/figures/IRBC_Matrix.png",
    width = 700,
    height = 700)

par(mfrow = c(4,4))
par(mar = c(2.5, 2.5, 2.5, 2.5))

IR_BC_plot(cloudModel)
text(0.5, 0.35, "Total.Cloud.Amount", cex = 1.2)

IR_BC_plot(precipModel)
text(0.5, -0.15, "Present.Weather.Numeric", cex = 1.2)

IR_BC_plot(pressureModel)
text(1012, 1004, "Sea.Level.Pressure", cex = 1.2)

IR_BC_plot(swellHeightModel)
text(10, 3, "Swell.Height^2", cex = 1.2)

IR_BC_plot(temperatureModel)
text(27.5, 20.5, "Air.Temperature")

IR_BC_plot(waveHeightModel)
text(1.75, 0.48, "(Wave.Height+.01)^0.5")

IR_BC_plot(windDirModel)
text(15, 8, "Wind.Direction^0.5")

IR_BC_plot(windSpeedModel)
text(17.5, 5, "Wind.Speed", cex = 1.2)

dev.off()


#C: FORECASTING:


#Index 55 (last observation):
finalObs <- marine[55,c(1,2,3,5,8,11,12,20,21,24)]
finalObs$Swell.Height <- marine$Swell.Height[53] #Last non-NA value

#Initial Prediction Vector:
initPredictors <- t(finalObs)
initPredictors <- as.data.frame(initPredictors)
colnames(initPredictors) <- "Day0"

#Initializing Lists:
lmModels <- list(pressureModel, temperatureModel, 
                waveHeightModel, swellHeightModel, 
                cloudModel, windDirModel, 
                windSpeedModel, precipModel)
variList <- list(pressureVars, temperatureVars,
                 waveHeightVars, swellHeightVars,
                 cloudVars, windDirVars,
                 windSpeedVars, precipVars)
regPowList <- list(pressureRegPowers, temperatureRegPowers,
                   waveHeightRegPowers, swellHeightRegPowers,
                   cloudRegPowers, windDirRegPowers,
                   windSpeedRegPowers, precipRegPowers)
resPowList <- list(pressureResPowers, temperatureResPowers,
                   waveHeightResPowers, swellHeightResPowers,
                   cloudResPowers, windDirResPowers,
                   windSpeedResPowers, precipResPowers)
offsetList <- list(pressureOffset, temperatureOffset,
                   waveHeightOffset, swellHeightOffset,
                   cloudOffset, windDirOffset,
                   windSpeedOffset, precipOffset)

#Days to Forecast, Latitude, and Longitude Locations:

#1. This simulates a 5-day voyage from marine[55] to Mayabunder, India
#(Realistic)

forecastDays <- 5
latiVals <- c(7.3, 8.5, 10, 11.5, 12.9)
longiVals <- c(94.2, 94, 93.3, 93, 92.9)


#Forecast Values of (1):

mayabunderMarineForecast <- forecastValues(initPredictors, lmModels, 
               variList, regPowList, resPowList, offsetList,
               latiVals, longiVals, forecastDays)

#2. This simulates a 5-day voyage from marine[55] to Augusta, Australia
#(Unrealistic)

forecastDays <- 5
latiVals <- c(0, -8.5, -17, -23.5, -34.3)
longiVals <- c(97.2, 101, 107, 110.5, 115.2)

#Forecast Values of (2):

augustaMarineForecast <- forecastValues(initPredictors, lmModels, 
                variList, regPowList, resPowList, offsetList,
                latiVals, longiVals, forecastDays)


#D: VISUALIZING PATHS:


#(1) Mayabunder, India Boat Path:

mayabunder <- as.data.frame(t(mayabunderMarineForecast))

png("../../volume/figures/mayabunderPath.png",
    width = 600,
    height = 360)

worldMap <- map_data("world")
ggplot() +
  geom_map(data = worldMap, 
           map = worldMap,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgreen") +
  xlim(90, 100) +
  ylim(0, 20) +
  geom_line(data = marine,
            aes(Longitude, Latitude),
            color = "red",
            lwd = 1) + 
  geom_point(data = mayabunder,
             aes(Longitude, Latitude),
             color = "blue",
             size = 2) +
  annotate("text", label = "Jan 12", color = "black",
           x = 3, y = -39, size = 3) + 
  annotate("text", label = "Jan 31", color = "black",
           x = 94.5, y = 5, size = 3) + 
  annotate("text", label = "Feb 5", color = "black",
           x = 93.5, y = 13, size = 3) +
  labs(title = "D5GN6 Buoy Path, To Mayabunder, India (5 Days)")

dev.off()

#(2) Augusta, Australia Boat Path:

augusta <- as.data.frame(t(augustaMarineForecast))

png("../../volume/figures/augustaPath.png",
    width = 600,
    height = 360)

worldMap <- map_data("world")
ggplot() +
  geom_map(data = worldMap, 
           map = worldMap,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgreen") +
  geom_line(data = marine,
            aes(Longitude, Latitude),
            color = "red",
            lwd = 1) + 
  geom_point(data = augusta,
             aes(Longitude, Latitude),
             color = "blue",
             size = 2) +
  annotate("text", label = "Jan 12", color = "black",
           x = 3, y = -39, size = 3) + 
  annotate("text", label = "Jan 31", color = "black",
           x = 90, y = 9, size = 3) + 
  annotate("text", label = "Feb 5", color = "black",
           x = 115, y = -38, size = 3) +
  labs(title = "D5GN6 Buoy Path, To Augusta, Austrialia (5 Days)")

dev.off()


#E: WEATHER FORECAST VALUE TABLES & MISC FIGURES:


library(gridExtra)

unitColumn <- as.data.frame(c("Degrees", "Degrees", "Millibars", "Degrees C",
                              "Meters", "Meters", "Cloud %", 
                              "Degrees-CW from N", "m/sec", "Rain Chance"))
colnames(unitColumn) <- c("Units")

#(1) 5 Days to Mayabunder, India:

mayabunderClean <- t(data.frame(lapply(mayabunder, function(x) round(x, 2))))
colnames(mayabunderClean) <- nameDateColumns(mayabunderClean)
mayabunderClean <- cbind(mayabunderClean, unitColumn)

png("../../volume/figures/mayabunderForecastData.png",
    width = 800,
    height = 600)

grid.table(mayabunderClean)

dev.off()

#(2) 5 Days to Augusta, Australia:

augustaClean <- t(data.frame(lapply(augusta, function(x) round(x, 2))))
colnames(augustaClean) <- nameDateColumns(augustaClean)
augustaClean <- cbind(augustaClean, unitColumn)

png("../../volume/figures/augustaForecastData.png",
    width = 800,
    height = 600)

grid.table(augustaClean)

dev.off()

#(3) Regressor Response Grid (Export):

regresGrid <- as.data.frame(matrix(NA, nrow = 8, ncol = 13))
rownames(regresGrid) <- c("Sea.Level.Pressure", "Air.Temperature", "Wave.Height",
                          "Swell.Height", "Total.Cloud.Amount", "Wind.Direction",
                          "Wind.Speed", "Present.Weather.Numeric")
colnames(regresGrid) <- c("Latitude", "Longitude", "Sea.Level.Pressure", 
                          "Air.Temperature", "Wave.Height", "Swell.Height", 
                          "Total.Cloud.Amount", "Wind.Direction",
                          "Wind.Speed", "Present.Weather.Numeric", "Sea*Air",
                          "RespPower", "RespOffset")

regresGrid <- as.data.frame(t(regresGrid)) #Easier for Reference

#Powers of Regressors, Response, & Offset:

regresGrid$Sea.Level.Pressure <- c("-", "-", "-", "1", "-", "-", "-",
                                   "-", "1,2", "-", "-", "1", "0")

regresGrid$Air.Temperature <- c("2", "-", "1", "-", "-", "-", "2",
                                "1", "-", "-", "-", "1", "0")

regresGrid$Wave.Height <- c("2", "-", "-", "-", "-", "-", "-",
                            "1", "1", "-", "-", "0.5", "0.01")

regresGrid$Swell.Height <- c("2", "-", "-", "-", "-", "-", "-",
                             "-", "1,2", "-", "-", "2", "0")

regresGrid$Total.Cloud.Amount <- c("-", "-", "-", "1", "-", "-", "-",
                                   "1", "-", "-", "-", "1", "0")

regresGrid$Wind.Direction <- c("2", "-", "-", "1", "-", "-", "-",
                               "-", "-", "-", "-", "0.5", "0")

regresGrid$Wind.Speed <- c("2", "-", "1", "1", "-", "-", "-",
                           "1", "-", "-", "1", "1", "0")

regresGrid$Present.Weather.Numeric <- c("2", "-", "-", "-", "-", "-", "-",
                                        "1", "1", "-", "-", "1", "0")

regresGrid <- as.data.frame(t(regresGrid)) #Convert Back

png("../../volume/figures/regressorResponseOffsetMatrix.png",
    width = 1600,
    height = 600)

grid.table(regresGrid)

dev.off()
