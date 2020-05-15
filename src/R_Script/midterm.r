Midterm
load("midterm2020.RData")

library(tidyr)

#1
##1
str(pums.sample)

##2
pums.sample$SEX <- as.character(pums.sample$SEX)
pums.sample$SEX <- ifelse(pums.sample$SEX=="1", "Male", "Female")

##3
pums.sample$MAR <- as.character(pums.sample$MAR)
pums.sample$MAR <- ifelse(pums.sample$MAR == "1", "Married", 
    ifelse(pums.sample$MAR == "2", "Widowed",
    ifelse(pums.sample$MAR == "3", "Divorced",
    ifelse(pums.sample$MAR == "4", "Separated", 
           "Never married or under 15 years old")       
           )                             
        )
    )
pums.sample$MAR <- as.factor(pums.sample$MAR)

##4
colSums(is.na(pums.sample))
colSums(is.na(pums.sample)) / 2672 * 100

##5
total <- sum(is.na(pums.sample$FER))
total

total_male <- sum(pums.sample$SEX=="Male")
male_fer <- sum(is.na(pums.sample$FER) & pums.sample$SEX=="Male")
total_male - male_fer

female_under15 <- sum(pums.sample$SEX=="Female" & pums.sample$AGEP<15)
female_fer_under15 <- sum(is.na(pums.sample$FER) & pums.sample$SEX=="Female" & pums.sample$AGEP < 15)
female_under15 - female_fer_under15

female_over50 <- sum(pums.sample$SEX=="Female" & pums.sample$AGEP>50)
female_fer_over50 <- sum(is.na(pums.sample$FER) & pums.sample$SEX=="Female" & pums.sample$AGEP > 50)
female_over50 - female_fer_over50

total - male_fer - female_fer_over50 - female_fer_under15

##6
summary(pums.sample)
pums.sample[pums.sample$PINCP <=0, ]
pums.sample[pums.sample$PINCP >500000, ]

##7
table(pums.sample$COW, pums.sample$SCHL)
prop.table(table(pums.sample$COW, pums.sample$SCHL), margin = 2) * 100

##8
pums.sample$age_income <- cut(pums.sample$AGEP, breaks = c(0, 29, 39, 49, 59, 85),
            labels = c("Under 30", "Under 40", "Under 50", "Under 60", "Over 60"),
            include.lowest = T)

aggregate(PINCP~age_income, pums.sample, mean)

##9
plot(x=pums.sample$WKHP, y=pums.sample$PINCP)

pums.sample <- pums.sample[,-11]

#2
library(stringr)

iris$id <-1:nrow(iris)

iris.tidy <- gather(iris, PM, Value, Sepal.Length:Petal.Width)
iris.tidy <- separate(iris.tidy, PM, into=c("Part", "Measure"))
iris.wide <- spread(iris.tidy, Measure, Value)
iris.wide <- iris.wide[,-2]
head(iris.wide, 12)
str(iris.wide)

iris.tidy <- iris.tidy[,-2]

head(iris.tidy, 15)
str(iris.tidy)

save(pums.sample ,iris.tidy, iris.wide, file = "st21400022.RData")
