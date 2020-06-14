load(url('https://github.com/hbchoi/SampleData/raw/master/adult.RData'))
library(ROCR)

set.seed(2020)
n_sample <- nrow(adult)
rgroup <- runif(n_sample)
adult.train <- subset(adult, rgroup <= 0.8)
adult.test <- subset(adult, rgroup > 0.8)

# build prediction model with occupation
tble <- table(adult.train$occupation, adult.train$income_mt_50k)
sv_model_job <- prop.table(tble, margin = 1)[,2]
sort(sv_model_job, decreasing = T)

adult.train$est_prob <- sv_model_job[adult.train$occupation]
adult.test$est_prob <- sv_model_job[adult.test$occupation]

threshold <- 1

adult.train$predict <- adult.train$est_prob > threshold
adult.test$predict <- adult.test$est_prob > threshold

conf.table <- table(pred = adult.train$predict, actual = adult.train$income_mt_50k)
accuracy <- sum(diag(conf.table)) / sum(conf.table)

conf.table2 <- table(pred = adult.test$predict, actual = adult.test$income_mt_50k)
accuracy2 <- sum(diag(conf.table2)) / sum(conf.table2)
accuracy
# o.7580
accuracy2
# 0.7636
precision <- conf.table2[2,2] / sum(conf.table2[2,])
recall <- conf.table2[2,2] / sum(conf.table2[,2])
precision
recall

# build prediction model with education
tble <- table(adult.train$education, adult.train$income_mt_50k)
sv_model_job <- prop.table(tble, margin = 1)[,2]
sort(sv_model_job, decreasing = T)

adult.train$est_prob <- sv_model_job[adult.train$education]
adult.test$est_prob <- sv_model_job[adult.test$education]

threshold <- 0.5

adult.train$predict <- adult.train$est_prob > threshold
adult.test$predict <- adult.test$est_prob > threshold

conf.table <- table(pred = adult.train$predict, actual = adult.train$income_mt_50k)
accuracy <- sum(diag(conf.table)) / sum(conf.table)

conf.table2 <- table(pred = adult.test$predict, actual = adult.test$income_mt_50k)
accuracy2 <- sum(diag(conf.table2)) / sum(conf.table2)
accuracy
## 0.7788
accuracy2
## 0.7827

# threshold 0.4
threshold <- 0.4
adult.train$predict <- adult.train$est_prob > threshold
adult.test$predict <- adult.test$est_prob > threshold
conf.table <- table(pred = adult.train$predict, actual = adult.train$income_mt_50k)
accuracy <- sum(diag(conf.table)) / sum(conf.table)

conf.table2 <- table(pred = adult.test$predict, actual = adult.test$income_mt_50k)
accuracy2 <- sum(diag(conf.table2)) / sum(conf.table2)
accuracy
## 0.7499
accuracy2
## 0.7578

# threshold 0.6
threshold <- 0.6
adult.train$predict <- adult.train$est_prob > threshold
adult.test$predict <- adult.test$est_prob > threshold
conf.table <- table(pred = adult.train$predict, actual = adult.train$income_mt_50k)
accuracy <- sum(diag(conf.table)) / sum(conf.table)

conf.table2 <- table(pred = adult.test$predict, actual = adult.test$income_mt_50k)
accuracy2 <- sum(diag(conf.table2)) / sum(conf.table2)
accuracy
## 0.7726
accuracy2
## 0.7771

# accuracy is high when threshold is 0.5
# Education is more accurate than occupation

plot(performance(prediction(adult.test$est_prob, adult.test$income_mt_50k), 'tpr', 'fpr'))
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}
calAUC(adult.train$est_prob, adult.train$income_mt_50k)
## 0.7169
calAUC(adult.test$est_prob, adult.test$income_mt_50k)
## 0.7182



load(url('https://github.com/hbchoi/SampleData/raw/master/insurance.RData'))
insurance$charges_log <- log10(insurance$charges)
hist(insurance$charges_log)
plot(density(insurance$charges_log))

set.seed(2018)
ncustomer <- nrow(insurance)
rgroup <- runif(ncustomer)

train.df <- subset(insurance, rgroup <= 0.8)
test.df <- subset(insurance, rgroup > 0.8)

# build prediction with smoker
sv_reg_smoker <- tapply(train.df$charges_log, train.df$smoker, mean)
train.df$pred_charges_log <- sv_reg_smoker[train.df$smoker]
train.df$error <- train.df$charges_log - train.df$pred_charges_log
test.df$pred_charges_log <- sv_reg_smoker[test.df$smoker]
test.df$error <- test.df$charges_log - test.df$pred_charges_log
MSE_train <- mean(train.df$error ** 2)
RMSE_train <- sqrt(MSE_train)
MSE_test <- mean(test.df$error ** 2)
RMSE_test <- sqrt(MSE_test)
RMSE_train
RMSE_test
sd(train.df$charges_log)
sd(test.df$charges_log)

RSS = sum(train.df$error ** 2)
RSS2 = sum(test.df$error ** 2)
SStot = sum((train.df$charges_log - mean(train.df$charges_log)) ** 2)
SStot2 = sum((test.df$charges_log - mean(test.df$charges_log)) ** 2)
Rsq = 1 - RSS/SStot
Rsq2 = 1 - RSS2/SStot2
Rsq
Rsq2

## region as input variable for prediction
sv_reg_region <- tapply(train.df$charges_log, train.df$region, mean)
train.df$pred_charges_log <- sv_reg_region[train.df$region]
train.df$error <- train.df$charges_log - train.df$pred_charges_log
test.df$pred_charges_log <- sv_reg_region[test.df$region]
test.df$error <- test.df$charges_log - test.df$pred_charges_log
MSE_train <- mean(train.df$error ** 2)
RMSE_train <- sqrt(MSE_train)
MSE_test <- mean(test.df$error ** 2)
RMSE_test <- sqrt(MSE_test)
RMSE_train
RMSE_test
RSS = sum(train.df$error ** 2)
RSS2 = sum(test.df$error ** 2)
SStot = sum((train.df$charges_log - mean(train.df$charges_log)) ** 2)
SStot2 = sum((test.df$charges_log - mean(test.df$charges_log)) ** 2)
Rsq = 1 - RSS/SStot
Rsq2 = 1 - RSS2/SStot2
Rsq
Rsq2

## age as input variable for prediction
train.df$age_group <- cut(train.df$age, breaks = c(0,20,30,40,50,60, Inf),
                          labels = c('under20', '20s', '30s', '40s', '50s', 'over60'),
                          right = F)
test.df$age_group <- cut(test.df$age, breaks = c(0,20,30,40,50,60, Inf),
                          labels = c('under20', '20s', '30s', '40s', '50s', 'over60'),
                          right = F)

sv_reg_age <- tapply(train.df$charges_log, train.df$age_group, mean)
train.df$pred_charges_log <- sv_reg_age[train.df$age_group]
train.df$error <- train.df$charges_log - train.df$pred_charges_log
test.df$pred_charges_log <- sv_reg_age[test.df$age_group]
test.df$error <- test.df$charges_log - test.df$pred_charges_log
MSE_train <- mean(train.df$error ** 2)
RMSE_train <- sqrt(MSE_train)
MSE_test <- mean(test.df$error ** 2)
RMSE_test <- sqrt(MSE_test)
RMSE_train
RMSE_test
RSS = sum(train.df$error ** 2)
RSS2 = sum(test.df$error ** 2)
SStot = sum((train.df$charges_log - mean(train.df$charges_log)) ** 2)
SStot2 = sum((test.df$charges_log - mean(test.df$charges_log)) ** 2)
Rsq = 1 - RSS/SStot
Rsq2 = 1 - RSS2/SStot2
Rsq
Rsq2
