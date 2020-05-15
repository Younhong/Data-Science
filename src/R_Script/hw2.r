load("homework2_2020.RData")

#1 Data Exploring and Cleaning

##1
str(cust.df)

##2
summary(cust.df)
library(stringr)
cust.df$sex <- as.factor(cust.df$sex)
cust.df$age <- as.integer(cust.df$age)

##3
cust.df$custid <- as.character(cust.df$custid)
cust.df$custid <- str_pad(cust.df$custid, width = 7, side = "left", pad = "0")
cust.df$custid <- paste0('c', cust.df$custid)

##4
colSums(is.na(cust.df))
colSums(is.na(cust.df)) / 1000 * 100

##5
(colSums(is.na(cust.df)) < 100) & (colSums(is.na(cust.df)) > 0)
sum(is.na(cust.df$housing.type) & is.na(cust.df$recent.move) & is.na(cust.df$num.vehicles))

##6
cust.df$is.employed <- as.character(cust.df$is.employed)
cust.df$is.employed <- ifelse(is.na(cust.df$is.employed), 'missing', 
                              ifelse(cust.df$is.employed == "FALSE", "not employed", "employed"))

##7
mean_income <- aggregate(Income ~ state.of.res, cust.df, mean)
colnames(mean_income) <- c("state.of.res", "mean.income")
median_income <- aggregate(Income ~ state.of.res, cust.df, median)
colnames(median_income) <- c("state.of.res", "median.income")
avg_income <- merge(mean_income, median_income)
head(avg_income)

##8
rownames(avg_income) <- avg_income$state.of.res
cust.df$avg <- avg_income[cust.df$state.of.res, ][2]
cust.df$avg <- unlist(cust.df$avg)
cust.df$Income <- ifelse(is.na(cust.df$Income), cust.df$avg, cust.df$Income)

##9
cust.df$income.relative <- cust.df$Income / cust.df$avg
cust.df <- cust.df[,-12]

##10, 11
summary(cust.df)
cust.df[order(cust.df$age, decreasing = T), ]

#2 Tidy Data

##1. tidy data
head(bankruptcy_df, 20)

##2.
library(tidyr)
bankruptcy_df <- bankruptcy_df[,-1]
bank <- spread(bankruptcy_df, index, rating)
head(bank)

##3
bank[,3:8] <- ifelse(bank[,3:8]=="P", 'Positive', ifelse(bank[,3:8] == "A", "Average", "Negative"))
bank$Class <- ifelse(bank$Class=="B", 'Bankrupty', 'Non-Bankruptcy')
save(cust.df, bank, file = "hw2.RData")