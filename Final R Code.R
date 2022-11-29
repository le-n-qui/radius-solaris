# Import dataset from csv file
setwd("C:\\Users\\chris\\Downloads\\School\\Fall 2022\\Math 261A - Regression Theory and Methods\\Final\\radius-solaris")

# Predicting Solar Radiation
# By Multiple Linear Regression

# First import dataset from the CSV file using RStudio
# Files -> Look for CSV file -> Click on file name -> Import Dataset -> Import
# A dataframe object, solar_radiation, 
# will be available in the global environment

# OPTIONAL: 
# Change filename with your local path to the CSV file
# Uncomment the code below to get dataframe from CSV file
library(readr)
filename = "solar-radiation.csv"
solar_radiation <- read_csv(filename)
solar_radiation <- solar.radiation

monthfactor <- factor(solar_radiation$Month)
contrasts(monthfactor) <-contr.treatment(4,base=1)
contrasts(monthfactor)

# Create a train/test split
set.seed(1)
sample <- sample(c(TRUE,FALSE), nrow(solar_radiation), 
                 replace = TRUE, prob = c(0.8,0.2))
train <- solar_radiation[sample,]
test <- solar_radiation[!sample,]

# Remove Night Time values

train <- train[train$Day.1.Night.0 == 1,]

# Save data series (columns) 
# into local variables
UNIX <- train$UNIXTime
Radiation <- train$Radiation
Temperature <- train$Temperature
Pressure <- train$Pressure
Humidity <- train$Humidity
WindDirection <- train$WindDirection.Degrees
Speed <- train$Speed
DayNight <- train$`Day=1/Night=0`
Month <- train$Month
Day <- train$Day
TimeOfDay <- train$TimeOfDay
TimeSunRise <- train$TimeSunRise
TimeSunSet <- train$TimeSunSet
TimeSinceSunRise <- TimeOfDay - TimeSunRise

# Create Factor for Month Variable
monthfactor <- factor(Month)
contrasts(monthfactor) <-contr.treatment(4,base=1)
contrasts(monthfactor)

# Fit linear model with all numeric variables
fit <- lm(Radiation ~ Temperature + Pressure + Humidity
          + WindDirection + Speed + UNIX +
            TimeOfDay + TimeSunRise + TimeSunSet)
summary(fit)
cor(train)

# Fit a model without night time points, and 
# with TimeSinceSunrise as the time variable
fit <- lm(Radiation ~ Temperature + Pressure + Humidity 
          + WindDirection + Speed + TimeSinceSunRise)
summary(fit)
cor(cbind(Temperature, Pressure, Humidity, 
          WindDirection, Speed, TimeSinceSunRise))

##################
# FITTING MODELS #
##################

# This model has best R^2 given simplicity in the predictors
fit <- lm(I(sqrt(Radiation)) ~ poly(Temperature-mean(Temperature),2) 
          + poly(Humidity-mean(Humidity),2) + poly(TimeSinceSunRise-mean(TimeSinceSunRise),2))
summary(fit)

plot(fitted.values(fit),rstandard(fit))
plot(fit)

# These models have poor R^2, but meet model assumptions
fit <- lm(I(sqrt(Radiation)) ~ Temperature + poly(Humidity-mean(Humidity),2))
fit <- lm(I(sqrt(Radiation)) ~ Temperature)
fit <- lm(I(sqrt(Radiation)) ~ I(Temperature^2))
summary(fit)

plot(fitted.values(fit),rstandard(fit))
qqnorm(rstandard(fit))
abline(0,1)

# With Transformations
logT <- I(log(Temperature))
logH <-I(log(Humidity))
sqrtWD <- I(sqrt(WindDirection))
sqrtS <- I(sqrt(Speed))


fit <- lm(I(sqrt(Radiation)) ~ poly(logT-mean(logT),1)
          + poly(logH-mean(logH),2)
          + poly(TimeSinceSunRise-mean(TimeSinceSunRise),2))

fit <- lm(I(sqrt(Radiation)) ~ poly(logT-mean(logT),1)
          + poly(logH-mean(logH),2))

summary(fit)
plot(rstandard(fit) ~ fitted.values(fit))
qqnorm(rstandard(fit))
abline(0,1)


# Compare Individual Transformations
fit <- lm(Radiation ~ Temperature + Pressure + Humidity + WindDirection + Speed + TimeSinceSunRise)

par(mfrow=c(2,5))
fitlog <- lm(I(sqrt(Radiation)) ~ Temperature + Pressure + Humidity + WindDirection + Speed + I(log(TimeSinceSunRise)))
qqnorm(rstandard(fitlog), main="Log")
abline(0,1)

fitinv <- lm(I(sqrt(Radiation)) ~ Temperature + Pressure + Humidity + WindDirection + Speed + I(1/(TimeSinceSunRise)))
qqnorm(rstandard(fitinv), main="Inverse")
abline(0,1)

fitsqrt <- lm(I(sqrt(Radiation)) ~ Temperature + Pressure + Humidity + WindDirection + Speed + I(sqrt(TimeSinceSunRise)))
qqnorm(rstandard(fitsqrt), main="Sqrt")
abline(0,1)

fit2 <- lm(I(sqrt(Radiation)) ~ Temperature + Pressure + Humidity + WindDirection + Speed + I((TimeSinceSunRise)^2))
qqnorm(rstandard(fit2), main="Square")
abline(0,1)

fit3 <- lm(I(sqrt(Radiation)) ~ Temperature + Pressure +  Humidity + WindDirection + Speed + I((TimeSinceSunRise)^3))
qqnorm(rstandard(fit3), main="Cube")
abline(0,1)

plot(rstandard(fitlog) ~ fitted.values(fitlog), main="Log")
plot(rstandard(fitinv) ~ fitted.values(fitinv), main="Inverse")
plot(rstandard(fitsqrt) ~ fitted.values(fitsqrt), main="Sqrt")
plot(rstandard(fit2) ~ fitted.values(fit2), main="Square")
plot(rstandard(fit3) ~ fitted.values(fit3), main="Cube")

summary(fit)
summary(fitlog)
summary(fitinv)
summary(fitsqrt)
summary(fit2)
summary(fit3)

# R^2, sqrt{MS_Res}
# 1, log, -1, sqrt, 2, 3
# Temperature
#.5143, .5176, .5156, .5166, .5061, .4936
# 6.458, 6.436, 6.449, 6.443, 6.513, 6.595
# Pressure
# .5143, .5143, .5047, .5143, .5143, .5143
# 6.458, 6.458, 6.522, 6.458, 6.458, 6.458
# Humidity
# .5143, .5201, .5231, .5172, .51,   .505
# 6.458, 6.42,  6.4,   6.439, 6.487, 6.5
# WindDirection
# .5143, .5129, .5086, .5158, .5107, .5092
# 6.458, 6.468, 6.496, 6.449, 6.482, 6.492
# Speed
# .5143, .5158, N/A, .5166, .5083, .505
# 6.458, 6.449, N/A, 6.443, 6.498, 6.52
#TimeSinceSunRise
# .5143, .4467, .4471, .47,   .5826, .6177
# 6.458, 6.893, 6.891, 6.746, 5.987, 5.73

###################################
# Best Individual Transformations #
###################################

logT <- I(log(Temperature))
invH <- I(1/(Humidity))
sqrtWD <- I(sqrt(WindDirection))
sqrtS <- I(sqrt(Speed))
TSSR3 <- I((TimeSinceSunRise)^3)

fit <- lm(I(sqrt(Radiation)) ~ logT + Pressure + invH + sqrtWD + sqrtS + TSSR3)
drop1(fit, I(sqrt(Radiation)) ~ logT + Pressure + invH + sqrtWD + sqrtS + TSSR3, test = "F")

fit <- lm(I(sqrt(Radiation)) ~ logT + invH + sqrtWD + sqrtS + TSSR3)
summary(fit)
par(mfrow=c(1,2))

qqnorm(rstandard(fit), main="Individual Transformations")
abline(0,1)

plot(rstandard(fit) ~ fitted.values(fit), main="Individual Transformations")

########################
# Polynomial Selection #
########################

fit <- lm(I(sqrt(Radiation)) ~ 
            poly(logT-mean(logT),2)
          + poly(Pressure-mean(Pressure),2) 
          + poly(invH-mean(invH),2) 
          + poly(sqrtWD-mean(sqrtWD),2) 
          + poly(sqrtS-mean(sqrtS),2) 
          + poly(TSSR3-mean(TSSR3),2))
summary(fit)

# Delete Insignificant Predictors (F test)
clogT <- logT-mean(logT)
cPressure <- Pressure-mean(Pressure)
cinvH <- invH-mean(invH)
csqrtWD <- sqrtWD-mean(sqrtWD)
csqrtS <- sqrtS-mean(sqrtS)
cTSSR3 <- TSSR3-mean(TSSR3)

fit <- lm(I(sqrt(Radiation)) ~ 
          clogT+ I(clogT^2)
          + cPressure + I(cPressure^2) 
          + cinvH + I(cinvH^2) 
          + csqrtWD + I(csqrtWD^2) 
          + csqrtS + I(csqrtS^2) 
          + cTSSR3 + I(cTSSR3^2))
drop1(fit, I(sqrt(Radiation)) ~ 
          clogT+ I(clogT^2)
          + cPressure + I(cPressure^2) 
          + cinvH + I(cinvH^2) 
          + csqrtWD + I(csqrtWD^2) 
          + csqrtS + I(csqrtS^2) 
          + cTSSR3 + I(cTSSR3^2),
      test = "F")
fit <- lm(I(sqrt(Radiation)) ~ 
            clogT+ I(clogT^2)
          + cPressure + I(cPressure^2) 
          + cinvH 
          + csqrtWD + I(csqrtWD^2) 
          + csqrtS + I(csqrtS^2) 
          + cTSSR3 + I(cTSSR3^2))
drop1(fit, I(sqrt(Radiation)) ~ 
        clogT+ I(clogT^2)
      + cPressure + I(cPressure^2) 
      + cinvH 
      + csqrtWD + I(csqrtWD^2) 
      + csqrtS + I(csqrtS^2) 
      + cTSSR3 + I(cTSSR3^2),
      test = "F")
summary(fit)
####################
# Final Polynomial #
####################
fit <- lm(I(sqrt(Radiation)) ~ 
            clogT+ I(clogT^2)
          + cPressure + I(cPressure^2) 
          + cinvH 
          + csqrtWD
          + csqrtS + I(csqrtS^2) 
          + cTSSR3 + I(cTSSR3^2))
drop1(fit, I(sqrt(Radiation)) ~ 
        clogT+ I(clogT^2)
      + cPressure + I(cPressure^2) 
      + cinvH 
      + csqrtWD
      + csqrtS + I(csqrtS^2) 
      + cTSSR3 + I(cTSSR3^2),
      test = "F")

par(mfrow=c(1,2))

qqnorm(rstandard(fit), main="Polynomial Individual Transformations")
abline(0,1)

plot(rstandard(fit) ~ fitted.values(fit), main="Polynomial Individual Transformations")

summary(fit)

library(leaps)
all <- regsubsets(x=cbind(logT, I(logT^2), 
                          Pressure, I(Pressure^2), 
                          invH, I(invH^2), 
                          sqrtWD, I(sqrtWD^2), 
                          sqrtS, I(sqrtS^2), 
                          TSSR3, I(TSSR3^2)),
                  y=Radiation, method = "exhaustive", 
                  all.best = FALSE, nbest = 3)
all <- regsubsets(x=cbind(logT,
                          Pressure,
                          invH,
                          sqrtWD,
                          sqrtS,
                          TSSR3),
                  y=Radiation, method = "exhaustive", 
                  all.best = FALSE, nbest = 3)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
cor(train[3:7])
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(length(Day)-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
colnames(output)[3:7] <- c("Temperature", "Pressure", "Humidity",
                           "Wind Direction", "Wind Speed") 
output


#####################
# Final Final Model #
#####################

# Add Month
fit <- lm(I(sqrt(Radiation)) ~ logT + Pressure + invH + sqrtWD + sqrtS + TSSR3 + monthfactor)

# PRESS Statistic and R^2 Prediction
library(MPV)
PRESS <- PRESS(fit)
SST <- sum(anova(fit)["Sum Sq"])
R2_Pred <- 1 - PRESS / SST
R2_Pred

par(mfrow=c(1,2))

qqnorm(rstandard(fit), main="Polynomial Individual Transformations")
abline(0,1)

plot(rstandard(fit) ~ fitted.values(fit), main="Polynomial Individual Transformations")

summary(fit)

# Predict Individual Slopes

# Leverage and Influence

# Predict the Average Solar Radiation In San Jose?

# Confidence Intervals












solar_radiation <- as.data.frame(solar_radiation)
model_seq <- seq()
r2_seq <- seq()
mse_seq <- seq()

for(i in c(3,4,5,6,7)) {
  for (y in c(1)){
    print(i)
    print(names(solar_radiation)[i])
    x <- poly(solar_radiation[,i])
    fit_tmp <-lm(solar_radiation$Radiation ~ x, data = solar_radiation)
    png(file=paste("residual_plot_for_", names(solar_radiation)[i], "^", y, ".png",sep = ""))
    plot(fitted.values(fit_tmp), rstandard(fit_tmp), main=paste("Residual Plot for", names(solar_radiation)[i], " and power to ", y,sep = ""))
    dev.off()
    jpeg(file=paste("qq_plot_for_", names(solar_radiation)[i], "^", y, ".png",sep = ""))
    plot(fitted.values(fit_tmp), rstandard(fit_tmp), main=paste("QQ Plot for", names(solar_radiation)[i], " and power to ", y,sep = ""))
    dev.off()
    sm <- summary(fit_tmp)
    sink(file = paste("summary_for_", names(solar_radiation)[i], "^", y,".txt",sep = ""))
    print(sm)
    sink()
    model_seq <- cbind(model_seq, paste(names(solar_radiation)[i], "^", y,sep = ""))
    r2_seq <- cbind(r2_seq,sm$r.squared)
    mse_seq <- cbind(mse_seq, mean(sm$residuals^2))
  }
}


# Order of Deletion
# Pressure,2
# logT, 2
# Pressure, 1
# sqrt????



solve(cor(cbind(Temperature, Pressure, Humidity, WindDirection, Speed, TimeSinceSunRise)))


# Plot residual plot
# to examine residual assumptions
plot(fitted.values(fit), residuals(fit), 
     xlab = "fitted values", ylab = "Studentized residuals",
     main = "residual plot")

qqnorm(rstandard(fit))
abline(0,-1, col = "red")

# Initial variable selection

library(leaps)
?regsubsets
all <- regsubsets(x=cbind(Temperature,Pressure, Humidity,WindDirection, Speed),
                  y=Radiation,method = "exhaustive", 
                  all.best = FALSE, nbest = 3)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
cor(train[3:7])
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(length(Day)-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
colnames(output)[3:7] <- c("Temperature", "Pressure", "Humidity",
                           "Wind Direction", "Wind Speed") 
output

# Box-Cox

library(MASS)
boxcox(fit, seq(.4,.6,.1))
?boxcox

# Attempt Ln(Radiation) transformation
fit <- lm(log(Radiation) ~ (Temperature + Pressure 
                            + Humidity + WindDirection + Speed) * DayNight)
summary(fit)

# Attempting a root transformation on response

fit <- lm(I(sqrt(Radiation)) ~ Temperature + Pressure + Humidity 
          + WindDirection + Speed)
summary(fit)

# Initial Plots
plot(Temperature,Radiation)
plot(Pressure, Radiation)
plot(Humidity, Radiation)
plot(WindDirection, Radiation)
plot(Speed, Radiation)
plot(DayNight, Radiation)
plot(TimeSinceSunRise, Radiation)
plot(transformed.Time, Radiation)
fit <- lm(Radiation~transformed.Time)
summary(fit)
transformed.Time <- sin(3.14/(TimeSinceSunRise[max(Radiation)] - 
                                TimeSinceSunRise[min(Radiation)] )* 
                          (TimeSinceSunRise - TimeSinceSunRise[min(Radiation)]))

sample <- sample(c(TRUE,FALSE), nrow(solar_radiation), 
                 replace = TRUE, prob = c(0.8,0.2))
train <- solar_radiation[sample,]

sample <- sample(c(1:nrow(solar_radiation)), .01*nrow(solar_radiation))
sample

plot(TimeSinceSunRise,transformed.Radiation.sample)

sample_0 <- sample_n(solar_radiation, length(solar_radiation)*0.01)
library(dplyr)


transformed.Time.sample <- sin(3.14/(TimeSinceSunRise[max(Radiation)] - 
                                       TimeSinceSunRise[min(Radiation)] )* 
                                 (TimeSinceSunRise - TimeSinceSunRise[min(Radiation)]))
transformed.Radiation.sample <- sin(3.14/(max(Radiation) - 
                                            min(Radiation) )* 
                                      (Radiation - min(Radiation)))
Time <- TimeSinceSunRise - mean(TimeSinceSunRise)
Time <- abs(Time)
plot(Time, Radiation)
fit <- lm(Radiation~Time + Temperature + Pressure + Humidity 
          + WindDirection + Speed)
summary(fit)

#Prediction on test

monthfactor <- factor(solar_radiation$Month)
class(final_data$MonthFactor)
final_data <- cbind(solar_radiation$Radiation, solar_radiation$Pressure,I(log(solar_radiation$Temperature))
                    ,I(1/(solar_radiation$Humidity)),I(sqrt(solar_radiation$WindDirection.Degrees.))
                    ,I(sqrt(solar_radiation$Speed)),I(((solar_radiation$TimeOfDay-solar_radiation$TimeSunRise)^3))
                    , factor(solar_radiation$Month),solar_radiation$Day.1.Night.0)
final_data <- as.data.frame(final_data)
names(final_data)[1] <- "Radiation"
names(final_data)[2] <- "Pressure"
names(final_data)[3] <- "LogTemperature"
names(final_data)[4] <- "InverseHumidity"
names(final_data)[5] <- "RootWindDirection"
names(final_data)[6] <- "RootWindSpeed"
names(final_data)[7] <- "CubedTimeSinceSunrise"
names(final_data)[8] <- "MonthFactor"
names(final_data)[9] <- "Day"
final_data[8] <- monthfactor

set.seed(1)
sample <- sample(c(TRUE,FALSE), nrow(final_data), 
                 replace = TRUE, prob = c(0.8,0.2))
train <- final_data[sample,]
test <- final_data[!sample,]

fit <- lm(Radiation ~ Pressure + LogTemperature + InverseHumidity + RootWindDirection + RootWindSpeed + 
            CubedTimeSinceSunrise + MonthFactor, data = train)
summary(fit)
predictions <- predict(fit,test)
predictions
prediction.accuracy <- 1 - sum((test$Radiation - predictions)^2)/sum((test$Radiation - mean(test$Radiation))^2)
prediction.accuracy
