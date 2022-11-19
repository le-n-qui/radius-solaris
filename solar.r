# Predicting Solar Radiation
# By Multiple Linear Regression

# Import dataset from CSV file
data <- read.csv('solar-radiation.csv', header = TRUE, sep = ",")

# Create a train/test split
set.seed(1)
sample <- sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[sample,]
test <- data[!sample,]

#Remove Night time values

train <- train[train$Day.1.Night.0 == 1,]

# Save data series (columns) 
# into local variables
UNIX <- train$UNIXTime
Radiation <- train$Radiation
Temperature <- train$Temperature
Pressure <- train$Pressure
Humidity <- train$Humidity
WindDirection <- train$WindDirection.Degrees.
Speed <- train$Speed
DayNight <- train$Day.1.Night.0
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

#All numeric variable model

fit <- lm(Radiation ~ Temperature + Pressure + Humidity + WindDirection + Speed + UNIX + TimeOfDay + TimeSunRise + TimeSunSet)
summary(fit)
cor(train)

#Model without night time points, and Time since surise as the time variable
fit <- lm(Radiation ~ Temperature + Pressure + Humidity + WindDirection + Speed + TimeSinceSunRise)
summary(fit)
cor(cbind(Temperature, Pressure, Humidity, WindDirection, Speed, TimeSinceSunRise))

# Fit with DayNight
fit <- lm(Radiation ~ (Temperature + Pressure + Humidity + WindDirection + Speed + TimeSinceSunRise) * DayNight)
summary(fit)

#Residual Assumptions
plot(fitted.values(fit), rstudent(fit), xlab = "fitted values", ylab = "Studentized residuals", main = "residual plot")
qqnorm(rstandard(fit))
abline(0,1)

#Initial variable Select

library(leaps)
?regsubsets
all <- regsubsets(x=cbind(Temperature,Pressure, Humidity, WindDirection, Speed), y=Radiation,  method = "exhaustive", all.best = FALSE, nbest = 3)
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
colnames(output)[3:7] <- c("Temperature", "Pressure", "Humidity", "Wind Direction", "Wind Speed") 
output

#Box-Cox

library(MASS)
boxcox(fit, seq(.4,.6,.1))
?boxcox

#Attempt Ln(Radiation) transformation
fit <- lm(log(Radiation) ~ (Temperature + Pressure + Humidity + WindDirection + Speed) * DayNight)
summary(fit)

#Attempting a root transformation on response

fit <- lm(Radiation^.5 ~ Temperature + Pressure + Humidity + WindDirection + Speed)
summary(fit)

# Initial Plots
plot(Temperature,Radiation)
plot(Pressure, Radiation)
plot(Humidity, Radiation)
plot(WindDirection, Radiation)
plot(Speed, Radiation)
plot(DayNight, Radiation)

