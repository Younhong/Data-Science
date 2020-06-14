#1
bmi <- read.csv('bmi_clean.csv', header = T)
head(bmi)
bmi_long <- gather(bmi, year, bmi_val, Y1980:Y2008)
head(bmi_long)
bmi_wide <- spread(bmi_long, year, bmi_val)
head(bmi_wide)

#2
bmi_cc <- read.csv(file = 'bmi_cc.csv', header = TRUE)
head(bmi_cc)
bmi_cc_clean <- separate(bmi_cc, Country_ISO, c("Country", "ISO"), sep="/")
head(bmi_cc_clean)
bmi_cc2 <- unite(bmi_cc_clean, Country_ISO, Country, ISO)
head(bmi_cc2)

#3
library(lubridate)
students2 <- read.csv(file = 'students2.csv', header = TRUE, stringsAsFactors = F)
str(students2)
students2$dob <- ymd(students2$dob)
students2$nurse_visit <- ymd_hms(students2$nurse_visit)
str(students2)

#4
library(stringr)
head(students2)
str_detect(students2$dob, "1997")
students2$sex <- str_replace(students2$sex,"Female", "F")
students2$sex <- str_replace(students2$sex,"Male", "M")
head(students2)

#5
students3 <- read.csv('students3.csv', header = TRUE, stringsAsFactors = F)
summary(students3)
hist(students3$age)
hist(students3$absences)
hist(students3$absences, right = FALSE)
boxplot(students3$age)
boxplot(students3$absences)