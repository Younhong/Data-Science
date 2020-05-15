#1
mtcars$kpl <- mtcars$mpg * 1.609344 / 3.785412

#2
mpgtop3 <- mtcars[order(mtcars$mpg), ]
head(mpgtop3, 3)
tail(mpgtop3, 3)

#3
aggregate(mpg~cyl, mtcars, FUN = mean)

#4
genesis <- c(8.5 * 3.785412 / 1.609344, 6, 170, 130, 3.9, 3.0, 20.1, 1, 1, 5, 4, 8.5)
mtcars <- rbind(mtcars, Genesis = genesis)

#5
aggregate(mpg~hp>100, mtcars, FUN = mean)

#6
mtcars[order(mtcars$mpg), ]
mtcars[order(mtcars$hp, decreasing = T), ]

#7
mtcars$kg <- mtcars$wt * 0.453592
heaviest <- mtcars[order(mtcars$kg, decreasing = T), ]
head(heaviest, 5)
lightest <- mtcars[order(mtcars$kg), ]
head(heaviest, 5)

#8
mean(heaviest$mpg)
mean(heaviest$hp)
mean(lightest$mpg)
mean(lightest$hp)

#9
mtcars$price <- 0

#10
write.csv(mtcars, 'mtcars_test.csv')

#11
save(mtcars, file = "mtcars_test.RData")