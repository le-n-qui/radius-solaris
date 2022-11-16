# Import dataset from csv file
data <- read.csv('solar-radiation-updated.csv', header = TRUE, sep = ",")
head(data,100)
#tetschun
# Data Columns
UNIX <- data$UNIXTime
Radiation <- data$Radiation
Temperature <- data$Temperature
Pressure <- data$Pressure
Humidity <- data$Humidity
WindDirection <- data$WindDirection.Degrees.
Speed <- data$Speed
DayNight <- data$Day.1.Night.0
Month <- data$Month
Day <- data$Day
TimeOfDay <- data$TimeOfDay
TimeSunRise <- data$TimeSunRise
TimeSunSet <- data$TimeSunSet

# Create Factor for Month Variable
monthfactor <- factor(Month)
contrasts(monthfactor) <-contr.treatment(4,base=1)
contrasts(monthfactor)

# Initial Plots
plot(Temperature,Radiation)
plot(Pressure, Radiation)
plot(Humidity, Radiation)
plot(WindDirection.Degrees., Radiation)
plot(Speed, Radiation)
plot(DayNight, Radiation)

# Fit
fit <- lm(Radiation ~ (Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed) * Day.1.Night.0)
summary(fit)
plot(fitted.values(fit), rstudent(fit), xlab = "fitted values", ylab = "Studentized residuals", main = "residual plot")
qqnorm(rstandard(fit))
abline(0,1)
set.seed(1)
sample <- sample(c(TRUE,FALSE), nrow(solar.radiation.updated),replace = TRUE, prob = c(0.8,0.2))
train <- solar.radiation.updated[sample,]
test <- solar.radiation.updated[!sample,]
fit <- lm(Radiation ~ (Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed) * Day.1.Night.0, data = train)
summary(fit)
plot(fitted.values(fit), rstudent(fit), xlab = "fitted values", ylab = "Studentized residuals", main = 
       "residual plot")
qqnorm(rstandard(fit))
abline(0,1)
library(leaps)
?regsubsets
all <- regsubsets(x=cbind(solar.radiation.updated$Temperature,solar.radiation.updated$Pressure, solar.radiation.updated$Humidity, solar.radiation.updated$WindDirection.Degrees., solar.radiation.updated$Speed), y=solar.radiation.updated$Radiation,  method = "exhaustive", all.best = FALSE, nbest = 3)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
cor(solar.radiation.updated[3:7])
