load("weather.RData")

#1#2
library(tidyr)
library(stringr)

data <- gather(weather, dayOfMonth, value, X1:X31)
data <- data[,-1]
data2 <- spread(data, measure, value)

#3
library(lubridate)
data2$dayOfMonth <- str_replace(data2$dayOfMonth, 'X', '')
weather2 <- unite(data2, date, year, month, dayOfMonth, sep="-")

weather2$date <- ymd(weather2$date)

#4
weather2$PrecipitationIn <- str_replace(weather2$PrecipitationIn, 'T', '0')
weather2$PrecipitationIn <- as.numeric(weather2$PrecipitationIn)

#5
# apply(mtcars, 2, sum), unlist(lapply(mtcars, sum)), sapply(mtcars, sum)

weather2[,c(-1,-3)] <- sapply(weather2[,c(-1,-3)], as.numeric)
# weather2[,c(-1,-3)] <- apply(weather2[,c(-1,-3)], 2, as.numeric)
# weather2[,c(-1,-3)] <- unlist(lapply(weather2[,c(-1,-3)], as.numeric))
weather2$Events <- as.factor(weather2$Events)

#6
sum(colSums(is.na(weather2)))
colSums(is.na(weather2))

#7
boxplot(weather2$Max.Humidity)
weather2$Max.Humidity <- ifelse(weather2$Max.Humidity > 100, weather2$Max.Humidity/10, weather2$Max.Humidity)
summary(weather2)

#8
boxplot(weather2$Mean.VisibilityMiles)
weather2$Mean.VisibilityMiles <- ifelse(weather2$Mean.VisibilityMiles < 0, NA, weather2$Mean.VisibilityMiles)
summary(weather2)

#9
weather2$Events <- ifelse(weather2$Events == "", 'None', weather2$Events)

#10
colnames(weather2) <- tolower(colnames(weather2))

save(weather2, file = 'weather2.RData')