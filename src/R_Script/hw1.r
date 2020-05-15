#import csv file with header
bank <- read.csv('bank_hw.csv', header = T)

#1
dim(bank)[1]
sum(bank$age < 30)
sum(bank$age > 50)

#2
bank$balance_kw <- bank$balance * 1200

#3
sum(bank$y == "yes")
sum(bank$y == "yes") / dim(bank)[1] * 100

#4
library(stringr)
bank$pdays <- str_replace(bank$pdays, "-1", "")
sum(is.na(as.numeric(bank$pdays)))

#5
have_job <- !is.na(bank$job)
job_category <- aggregate(have_job ~ job, bank, FUN = sum)
print(job_category)

#6
age_zone <- c('Under 20', '20~29', '30~39', '40~49', '50~59', 'over 60')
bank$age_group <- cut(bank$age, breaks = c(0,19,29,39,49,59,100), labels = age_zone)
yes <- bank$y == "yes"
age_group_yes <- aggregate(yes ~ age_group, bank, FUN = sum)
age_group_yes[which.max(age_group_yes$yes), ]

#7
total <- !is.na(bank$y)
age_group_total <- aggregate(total ~ age_group, bank, FUN = sum)
age_group_data <- merge(age_group_yes, age_group_total)
age_group_data$percent <- age_group_data$yes / age_group_data$total * 100
age_group_data[which.max(age_group_data$percent), ]

#8
aggregate(duration ~ contact, bank, FUN = mean)

#9
bank <- bank[order(bank$age, decreasing = T), ]
head(bank)

#10
save(bank, file = 'bank.RData')