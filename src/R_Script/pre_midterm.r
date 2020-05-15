# Pre-Midterm
##1
insurance <- read.csv('insurance.csv', header = T)
head(insurance)

##2
str(insurance)

##3
insurance$bmi_level <- cut(insurance$bmi, 
      breaks=c(0, 18.5, 24.9, 54), 
      include.lowest = T, labels=c('light', 'normal', 'heavy'))

aggregate(charges~bmi_level, insurance, mean)

##4
male <- insurance[insurance$sex=="male", ]
female <- insurance[insurance$sex=="female", ]
summary(male)
summary(female)

##5
hist(insurance$charges)

##6
aggregate(charges~smoker, insurance, mean)

##7
aggregate(charges~children, insurance, mean)

##8
cut_points <- quantile(insurance$age, c(0, 0.1, 0.9, 1))
insurance$age_charge <- cut(insurance$age, breaks = cut_points,
                            include.lowest = T, 
                            labels = c('young10%', 'center', 'old10%'))
ten_percent <- aggregate(charges~age_charge,insurance, mean)
ten_percent
ten_percent[3,2] - ten_percent[1,2]

##9
aggregate(charges~sex, insurance, mean)

##10
total_sex <- !is.na(insurance$sex)
total <- aggregate(total_sex~sex, insurance, sum)

smoker2<- insurance[insurance$smoker=="yes",]
total_smoker <- !is.na(smoker2$sex)
total2 <- aggregate(total_smoker~sex, smoker2, sum)

smoke_sex_prop <- merge(total, total2)
smoke_sex_prop$prop <- smoke_sex_prop$total_smoker / smoke_sex_prop$total_sex * 100
smoke_sex_prop

#2
auto <- read.table('automobile.tsv', stringsAsFactors = F, header = T)

##1
head(auto)

##2
library(tidyr)
auto2 <- spread(auto, specification, value)
head(auto2)
save(auto2, file = 'automobile.RData')
