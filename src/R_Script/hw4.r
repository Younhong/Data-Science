PRSA <- read.csv(file = 'PRSA_data.csv')
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(class)

#1
PRSA <- na.omit(PRSA)
PRSA$bad_air <- ifelse(PRSA$pm2.5 > 75, T, F)

prsa.train <- subset(PRSA, year <= 2013)
prsa.test <- subset(PRSA, year > 2013)

#2
prsa.train$dewp_group <- cut(prsa.train$DEWP, breaks = c(-40, -20, 0, 20, 40),
                             labels = c('-40 ~ -20', '-20 ~ 0', '0 ~ 20', '20 ~ 40'),
                             right = F)
prsa.train$temp_group <- cut(prsa.train$TEMP, breaks = c(-20, 0, 20, 40, 60),
                             labels = c('-20 ~ 0', '0 ~ 20', '20 ~ 40', '40 ~ 60'),
                             right = F)
prsa.train$pres_group <- cut(prsa.train$PRES, breaks = c(990, 1010, 1030, 1050),
                             labels = c('1', '2', '3'),
                             right = F)
prsa.train$is <- as.numeric(prsa.train$Is)
prsa.train$is_group <- cut(prsa.train$is, breaks = c(0, 10.0, 20.0, 30.0),
                           labels = c('1', '2', '3'),
                           right = F)
prsa.train$iws_group <- cut(prsa.train$Iws, breaks = c(0, 100, 200, 300, 400, 500, 600),
                            labels = c('1', '2', '3', '4', '5', '5'),
                            right = F)
prsa.train$ir_group <- cut(prsa.train$Ir, breaks = c(0, 10, 20, 30, 40),
                           labels = c('1', '2', '3', '4'),
                           right = F)
prsa_model <- rpart(bad_air ~ 
        dewp_group+temp_group+pres_group+is_group+iws_group+ir_group+cbwd, 
        data = prsa.train, method = "class",
                    control = rpart.control(cp = 0))
rpart.plot(prsa_model)
prsa_model

# Accuracy is highest on cbwd
prsa.train$pred <- predict(prsa_model,prsa.train,type='class')
conf_table_train <- table(pred = prsa.train$pred, 
                          actual = prsa.train$bad_air)

accuracy_train <- mean(prsa.train$bad_air == prsa.train$pred) 
precision_train <- conf_table_train[2,2] / sum(conf_table_train[2,])
recall_train <- conf_table_train[2,2] / sum(conf_table_train[,2])
f1_measure <- function(precision, recall) {
  2 * precision * recall / (precision + recall)
}

f1_train <- f1_measure(precision_train, recall_train)
accuracy_train
precision_train
recall_train
f1_train

#3
prsa.test$dewp_group <- cut(prsa.test$DEWP, breaks = c(-40, -20, 0, 20, 40),
                             labels = c('-40 ~ -20', '-20 ~ 0', '0 ~ 20', '20 ~ 40'),
                             right = F)
prsa.test$temp_group <- cut(prsa.test$TEMP, breaks = c(-20, 0, 20, 40, 60),
                             labels = c('-20 ~ 0', '0 ~ 20', '20 ~ 40', '40 ~ 60'),
                             right = F)
prsa.test$pres_group <- cut(prsa.test$PRES, breaks = c(990, 1010, 1030, 1050),
                             labels = c('1', '2', '3'),
                             right = F)
prsa.test$is <- as.numeric(prsa.test$Is)
prsa.test$is_group <- cut(prsa.test$is, breaks = c(0, 10.0, 20.0, 30.0),
                           labels = c('1', '2', '3'),
                           right = F)
prsa.test$iws_group <- cut(prsa.test$Iws, breaks = c(0, 100, 200, 300, 400, 500, 600),
                            labels = c('1', '2', '3', '4', '5', '5'),
                            right = F)
prsa.test$ir_group <- cut(prsa.test$Ir, breaks = c(0, 10, 20, 30, 40),
                           labels = c('1', '2', '3', '4'),
                           right = F)
prsa.test$pred <- predict(prsa_model,prsa.test,type='class') 
conf.table_test <- table(pred = prsa.test$pred, 
                         actual = prsa.test$bad_air)
accuracy_test <- mean(prsa.test$bad_air == prsa.test$pred) 
accuracy_test
# Not an overfitting since train data  is accurate enough to predict test data

#4
# Not an overfitting

#5
# Not an overfitting

#6
# False negative costs more
# 

#7
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}
prsa.train$pred <- predict(prsa_model,prsa.train,type='prob')[,2]
prsa.test$pred <- predict(prsa_model,prsa.test,type='prob')[,2]
plot(performance(prediction(prsa.train$pred, 
                            prsa.train$bad_air),'tpr', 'fpr'), 
     main="ROC")
plot(performance(prediction(prsa.test$pred, 
                            prsa.test$bad_air),'tpr', 'fpr'), 
     main="ROC")
calAUC(prsa.train$pred, prsa.train$bad_air)
calAUC(prsa.test$pred, prsa.test$bad_air)

#8
prsa.train <- prsa.train[,-1]
prsa.test <- prsa.test[,-1]

minmax_norm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

prsa.train2 <- 
  cbind(prsa.train, predict(
    dummyVars(~cbwd, data=prsa.train), prsa.train))
prsa.test2 <- 
  cbind(prsa.test, predict(
    dummyVars(~cbwd, data=prsa.test), prsa.test))

prsa_train_norm <- sapply(prsa.train2[,c(6:8,10:12,22:25)], minmax_norm)
prsa_test_norm <- sapply(prsa.test2[,c(6:8,10:12,22:25)], minmax_norm)

#9
prsa_train_label <- prsa.train2$bad_air
prsa_test_label <- prsa.test2$bad_air
sqrt(nrow(prsa_train_norm))
prsa_test_pred <- 
  knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
      k = 360)
knn_acc <- mean(prsa_test_label == prsa_test_pred)
cmat <- table(prsa_test_label, prsa_test_pred)
prsa_knn_prec <- cmat[2,2] / sum(cmat[,2])
prsa_knn_rec <- cmat[2,2] / sum(cmat[2,])
f1_knn <- f1_measure(prsa_knn_prec, prsa_knn_rec)
knn_acc
prsa_knn_prec
prsa_knn_rec
f1_knn

#10
getACC <- function(i) {
  prsa_test_pred <- 
    knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
        k = i)
  mean(prsa_test_label == prsa_test_pred)
}
getPrec <- function(i) {
  prsa_test_pred <- 
    knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
        k = i)
  cmat <- table(prsa_test_label, prsa_test_pred)
  cmat[2,2] / sum(cmat[,2]) 
}
getRecall <- function(i) {
  prsa_test_pred <- 
    knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
        k = i)
  
  cmat <- table(prsa_test_label, prsa_test_pred)
  cmat[2,2] / sum(cmat[2,])
}
getF1 <- function(i) {
  prsa_test_pred <- 
    knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
        k = i)
  cmat <- table(prsa_test_label, prsa_test_pred)
  prsa_knn_prec <- cmat[2,2] / sum(cmat[,2])
  prsa_knn_rec <- cmat[2,2] / sum(cmat[2,])
  f1_measure(prsa_knn_prec, prsa_knn_rec)
}
getAUC <- function(i) {
  prsa_test_pred <- 
    knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
        k = i, prob = TRUE)
  
  prsa_test_pred_prob <- 
    ifelse(prsa_test_pred==TRUE, attributes(prsa_test_pred)$prob, 
           1 - attributes(prsa_test_pred)$prob)
  calAUC(prsa_test_pred_prob, prsa_test_label)
}

kList <- seq(180, 490, 10)
acc_list <- sapply(kList, getACC)
prec_list <- sapply(kList, getPrec)
rec_list <- sapply(kList, getRecall)
F1_list <- sapply(kList, getF1)
AUC_list <- sapply(kList, getAUC)
plot(x=kList, y=acc_list, xlab="K", ylab="Accuracy", type="l")
plot(x=kList, y=prec_list, xlab="K", ylab="Precision", type="l")
plot(x=kList, y=rec_list, xlab="K", ylab="Recall", type="l")
plot(x=kList, y=F1_list, xlab="K", ylab="F1", type="l")
plot(x=kList, y=AUC_list, xlab="K", ylab="AUC", type="l")

#11
## k = 180
prsa_train_label <- prsa.train$bad_air
prsa_test_label <- prsa.test$bad_air
prsa_test_pred <- 
  knn(train = prsa_train_norm, test = prsa_test_norm, cl = prsa_train_label,
      k = 180, prob = TRUE)

threshold <- 0.05

thr_seq <- seq(0.01, 0.15, 0.01)
get_high_thres_recall <- function(i) {
  threshold <- i
  prsa_test_pred_prob <- 
    ifelse(prsa_test_pred==TRUE, attributes(prsa_test_pred)$prob, 
           1 - attributes(prsa_test_pred)$prob)
  prsa_test_pred_new <- ifelse(
    prsa_test_pred_prob > threshold, "T", "F")
  cmat <- table(prsa_test_label, prsa_test_pred_new)
  cmat[2,2] / sum(cmat[2,])  
}
thres_result <- sapply(thr_seq, get_high_thres_recall)
plot(thr_seq, thres_result, type="l")
