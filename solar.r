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
