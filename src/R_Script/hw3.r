PRSA <- read.csv(file = 'PRSA_data.csv')
PRSA <- na.omit(PRSA)

prsa.train <- subset(PRSA, year <= 2013)
prsa.test <- subset(PRSA, year > 2013)

#1
plot(prsa.train$TEMP, prsa.train$DEWP)
#### increase->increase

plot(prsa.train$DEWP, prsa.train$PRES)
#### increase, decrease
plot(prsa.train$Iws, prsa.train$Is)
#### Is is low regardless of ws

plot(prsa.train$TEMP, prsa.train$month)
#### increase, decrease

plot(prsa.train$Ir, prsa.train$month)
#### increase, decrease
plot(prsa.train$Is, prsa.train$month)
#### increase, decrease
plot(prsa.train$Iws, prsa.train$month)
#### increase, decrease

plot(prsa.train$TEMP, prsa.train$Is)
#### Is is mostly low, high is: temp between -10 to 0.
plot(prsa.train$TEMP, prsa.train$Iws)
#### iws max: temp -10

plot(prsa.train$Iws, prsa.train$DEWP)
#### mostly Iws is located low level, high iws: low DEWP

#2

### cbwd
sv_reg_cbwd <- tapply(prsa.train$pm2.5, prsa.train$cbwd, mean)
prsa.train$pred_pm <- sv_reg_cbwd[prsa.train$cbwd]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean(prsa.train$error ** 2))
##88.6775

### DEWP
summary(prsa.train$DEWP)
prsa.train$dewp_group <- cut(prsa.train$DEWP, breaks = c(-40, -20, 0, 20, 40),
                             labels = c('-40 ~ -20', '-20 ~ 0', '0 ~ 20', '20 ~ 40'),
                             right = F)
sv_reg_dewp <- tapply(prsa.train$pm2.5, prsa.train$dewp_group, mean)
prsa.train$pred_pm <- sv_reg_dewp[prsa.train$dewp_group]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean(prsa.train$error ** 2))
##89.61

### TEMP
summary(prsa.train$TEMP)
prsa.train$temp_group <- cut(prsa.train$TEMP, breaks = c(-20, 0, 20, 40, 60),
                             labels = c('-20 ~ 0', '0 ~ 20', '20 ~ 40', '40 ~ 60'),
                             right = F)
sv_reg_temp <- tapply(prsa.train$pm2.5, prsa.train$temp_group, mean)
prsa.train$pred_pm <- sv_reg_temp[prsa.train$temp_group]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean(prsa.train$error ** 2))
## 91.47

### PRES
summary(prsa.train$PRES)
prsa.train$pres_group <- cut(prsa.train$PRES, breaks = c(990, 1010, 1030, 1050),
                             labels = c('1', '2', '3'),
                             right = F)
sv_reg_pres <- tapply(prsa.train$pm2.5, prsa.train$pres_group, mean)
prsa.train$pred_pm <- sv_reg_pres[prsa.train$pres_group]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean((prsa.train$error ** 2)))
## 90.702

### Is
summary(prsa.train$Is)
prsa.train$is <- as.numeric(prsa.train$Is)
prsa.train$is_group <- cut(prsa.train$is, breaks = c(0, 10.0, 20.0, 30.0),
                             labels = c('1', '2', '3'),
                             right = F)

sv_reg_is <- tapply(prsa.train$pm2.5, prsa.train$is_group, mean)
prsa.train$pred_pm <- sv_reg_pres[prsa.train$is_group]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean((prsa.train$error ** 2)))
## 91.65

### Iws
summary(prsa.train$Iws)
prsa.train$iws_group <- cut(prsa.train$Iws, breaks = c(0, 100, 200, 300, 400, 500, 600),
                            labels = c('1', '2', '3', '4', '5', '5'),
                            right = F)
sv_reg_iws <- tapply(prsa.train$pm2.5, prsa.train$iws_group, mean)
prsa.train$pred_pm <- sv_reg_iws[prsa.train$iws_group]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean(prsa.train$error ** 2))
## 89.64

### Ir
summary(prsa.train$Ir)
prsa.train$ir_group <- cut(prsa.train$Ir, breaks = c(0, 10, 20, 30, 40),
                            labels = c('1', '2', '3', '4'),
                            right = F)
sv_reg_ir <- tapply(prsa.train$pm2.5, prsa.train$ir_group, mean)
prsa.train$pred_pm <- sv_reg_ir[prsa.train$ir_group]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
sqrt(mean(prsa.train$error ** 2))
## 91.54

### single Variable: cbwd

#3
sv_reg_cbwd <- tapply(prsa.train$pm2.5, prsa.train$cbwd, mean)
prsa.train$pred_pm <- sv_reg_cbwd[prsa.train$cbwd]
prsa.train$error <- prsa.train$pm2.5 - prsa.train$pred_pm
MSE_train <- mean(prsa.train$error ** 2)
RMSE_train <- sqrt(MSE_train)
RMSE_train #89.67
RSS <- sum(prsa.train$error ** 2)
SStot = sum((prsa.train$pm2.5 - mean(prsa.train$pm2.5)) ** 2)
Rsq = 1 - RSS/SStot 
Rsq #0.0639

#4
prsa.test$pred_pm <- sv_reg_cbwd[prsa.test$cbwd]
prsa.test$error <- prsa.test$pm2.5 - prsa.test$pred_pm
MSE_test <- mean(prsa.test$error ** 2)
RMSE_test <- sqrt(MSE_test)
RMSE_test #92.07
RSS <- sum(prsa.test$error ** 2)
SStot = sum((prsa.test$pm2.5 - mean(prsa.test$pm2.5)) ** 2)
Rsq = 1 - RSS/SStot 
Rsq #0.03

#5
## Not an overfitting

#6
## similar

#6
plot(y=prsa.train$pm2.5, x=prsa.train$pred_pm)
plot(y=prsa.test$pm2.5, x=prsa.test$pred_pm)

##################
##### Part 2 #####
##################

load(file = "bankruptcy.RData")
library(ROCR)

calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}
measure <- function(precision, recall) {
  m <- 2 * precision * recall / (precision + recall)
  print(m)
}

#1
## Industriy Risk?
tble <- table(bankruptcy_train$`Industrial Risk`, bankruptcy_train$Class)
sv_model_ir <- prop.table(tble, margin = 1)[,2]

bankruptcy_train$est_prob <- sv_model_ir[bankruptcy_train$`Industrial Risk`]
bankruptcy_test$est_prob <- sv_model_ir[bankruptcy_test$`Industrial Risk`]
calAUC(bankruptcy_train$est_prob, bankruptcy_train$Class)
##0.6352

## Management Risk?
tble <- table(bankruptcy_train$`Management Risk`, bankruptcy_train$Class)
sv_model_mr <- prop.table(tble, margin = 1)[,2]

bankruptcy_train$est_prob <- sv_model_mr[bankruptcy_train$`Management Risk`]
bankruptcy_test$est_prob <- sv_model_mr[bankruptcy_test$`Management Risk`]
calAUC(bankruptcy_train$est_prob, bankruptcy_train$Class)
##0.7049

## Financial Flexibility
tble <- table(bankruptcy_train$`Financial Flexibility`, bankruptcy_train$Class)
sv_model_ff <- prop.table(tble, margin = 1)[,2]

bankruptcy_train$est_prob <- sv_model_ff[bankruptcy_train$`Financial Flexibility`]
bankruptcy_test$est_prob <- sv_model_ff[bankruptcy_test$`Financial Flexibility`]
calAUC(bankruptcy_train$est_prob, bankruptcy_train$Class)
##0.9002

## Credibility
tble <- table(bankruptcy_train$Credibility, bankruptcy_train$Class)
sv_model_cr <- prop.table(tble, margin = 1)[,2]

bankruptcy_train$est_prob <- sv_model_cr[bankruptcy_train$Credibility]
bankruptcy_test$est_prob <- sv_model_cr[bankruptcy_test$Credibility]
calAUC(bankruptcy_train$est_prob, bankruptcy_train$Class)
##0.9202

## Competitiveness
tble <- table(bankruptcy_train$Competitiveness, bankruptcy_train$Class)
sv_model_co <- prop.table(tble, margin = 1)[,2]

bankruptcy_train$est_prob <- sv_model_co[bankruptcy_train$Competitiveness]
bankruptcy_test$est_prob <- sv_model_co[bankruptcy_test$Competitiveness]
calAUC(bankruptcy_train$est_prob, bankruptcy_train$Class)
##0.9923

## Operating Risk
tble <- table(bankruptcy_train$`Operating Risk`, bankruptcy_train$Class)
sv_model_or <- prop.table(tble, margin = 1)[,2]

bankruptcy_train$est_prob <- sv_model_or[bankruptcy_train$`Operating Risk`]
bankruptcy_test$est_prob <- sv_model_or[bankruptcy_test$`Operating Risk`]
calAUC(bankruptcy_train$est_prob, bankruptcy_train$Class)
##0.6523

## Competitiveness

#2
calAUC(bankruptcy_test$est_prob, bankruptcy_test$Class)
##AUC:1

#3
## not an overfitting

#4

fun_precision <-function(threshold, num) {
  bankruptcy_train$prediction <- 
  bankruptcy_train$est_prob > threshold
  bankruptcy_test$prediction <- 
    bankruptcy_test$est_prob > threshold
  conf.table <- table(pred = bankruptcy_train$prediction,
                      actual = bankruptcy_train$Class)
  precision_train <- conf.table[2,2] / sum(conf.table[2,])
  recall_train <- conf.table[2,2] / sum(conf.table[,2])
  
  conf.table2 <- table(pred = bankruptcy_test$prediction,
                      actual = bankruptcy_test$Class)
  precision_test <- conf.table2[2,2] / sum(conf.table2[2,])
  recall_test <- conf.table2[2,2] / sum(conf.table2[,2])
  
  if (num==1) return(precision_train)
  if (num==2) return(recall_train)
  if (num==3) return(precision_test)
  if (num==4) return(recall_test)
}

#### 0~1 threshold
plot(performance(prediction(bankruptcy_train$est_prob, 
                            bankruptcy_train$Class),'prec', 'cutoff'))
plot(performance(prediction(bankruptcy_train$est_prob, 
                            bankruptcy_train$Class),'rec', 'cutoff'))

plot(performance(prediction(bankruptcy_test$est_prob, 
                            bankruptcy_test$Class),'prec', 'cutoff'))
plot(performance(prediction(bankruptcy_test$est_prob, 
                            bankruptcy_test$Class),'rec', 'cutoff'))


#5
threshold <- 0.01
prec_train <- fun_precision(threshold, 1)
rec_train <- fun_precision(threshold, 2)
prec_test <- fun_precision(threshold, 3)
rec_test <- fun_precision(threshold, 4)
prec_train
rec_train
prec_test
rec_test

measure(prec_train, rec_train)
measure(prec_test, rec_test)

plot(performance(prediction(bankruptcy_test$est_prob, 
                            bankruptcy_test$Class),'f', 'cutoff'))


# threshold -> train test
# 0.01 ~ 0.90 -> 0.9819 1
# 0.91 ~ 0.9 -> 0.7888 0.7407

#6
?performance
plot(performance(prediction(bankruptcy_train$est_prob, 
                            bankruptcy_train$Class),'tpr', 'fpr'))
plot(performance(prediction(bankruptcy_test$est_prob, 
                            bankruptcy_test$Class),'tpr', 'fpr'))

#7
plot(performance(prediction(bankruptcy_train$est_prob, 
                            bankruptcy_train$Class),'acc', 'cutoff'))

#8
## Same auc