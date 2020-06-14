#1
car_df <- read.csv('cars04.csv', header = T)

#2
str(car_df)

#3
car_df$name <- as.character(car_df$name)

#4
mean(car_df$msrp - car_df$dealer_cost)

#5
car_df[which.max(car_df$city_mpg),]
car_df[which.max(car_df$city_mpg),]$city_mpg - car_df[which.max(car_df$city_mpg),]$hwy_mpg
car_df[which.max(car_df$city_mpg),]$name == car_df[which.max(car_df$hwy_mpg),]$name

#6
sapply(car_df[,2:6], sum)
sum(apply(car_df[,2:6], 1, sum) == 0)
# sum(car_df$sports_car == FALSE & car_df$suv == FALSE & car_df$wagon == FALSE & car_df$minivan == FALSE & car_df$pickup == FALSE)

#7
suv_avg_weight <- aggregate(weight ~ suv, car_df, FUN = mean)[2,2]
minivan_avg_weight<- aggregate(weight ~ minivan, car_df, FUN = mean)[2,2]
(minivan_avg_weight > suv_avg_weight) == TRUE

library(tidyr)
car_mini <- car_df[,c(1:6,16)]
car_mini2 <- gather(car_mini, car_type, value, sports_car:pickup)
car_mini2 <- car_mini2[car_mini2$value, -4]
aggregate(weight~car_type, car_mini2, mean)

mean(car_df[car_df$suv, 'weight'])
mean(car_df[car_df$minivan, 'weight'])

#8
car_df$avg_mpg <- (car_df$city_mpg + car_df$hwy_mpg)/2

#9
eco_grade_eval <- c('Bad', 'Normal', 'Good')
cut_points <- quantile(car_df$avg_mpg, probs = c(0, 0.2, 0.8, 1), na.rm = T)
car_df$eco_grade <- cut(car_df$avg_mpg, breaks = cut_points, include.lowest = T, labels = eco_grade_eval)

##1
load('anscombe.RData')
sapply(anscombe, mean)
sapply(anscombe, sd)

##2
plot(x=anscombe$x1, y=anscombe$y1)
plot(x=anscombe$x2, y=anscombe$y2)
plot(x=anscombe$x3, y=anscombe$y3)
plot(x=anscombe$x4, y=anscombe$y4)

##3
x_data <-gather(anscombe[,1:4], key.x, value.x, x1:x4)
y_data <-gather(anscombe[,5:8], key.y, value.y, y1:y4)
ans.tidy <- cbind(x_data, y_data)

anscombe.tidy1 <- gather(anscombe, key.x, value.x, x1)
anscombe.tidy1 <- gather(anscombe.tidy1, key.y, value.y, y1)
anscombe.tidy2 <- anscombe.tidy1[1:6]
anscombe.tidy1 <- anscombe.tidy1[7:10]
anscombe.tidy2 <- gather(anscombe.tidy2, key.x, value.x, x2)
anscombe.tidy2 <- gather(anscombe.tidy2, key.y, value.y, y2)
anscombe.tidy3 <- anscombe.tidy2[1:4]
anscombe.tidy2 <- anscombe.tidy2[5:8]
anscombe.tidy3 <- gather(anscombe.tidy3, key.x, value.x, x3)
anscombe.tidy3 <- gather(anscombe.tidy3, key.y, value.y, y3)
anscombe.tidy4 <- anscombe.tidy3[1:2]
anscombe.tidy3 <- anscombe.tidy3[3:6]
anscombe.tidy4 <- gather(anscombe.tidy4, key.x, value.x, x4)
anscombe.tidy4 <- gather(anscombe.tidy4, key.y, value.y, y4)
anscombe.tidy <- rbind(anscombe.tidy1, anscombe.tidy2, anscombe.tidy3, anscombe.tidy4)
