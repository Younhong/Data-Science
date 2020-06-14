load('hw5_student.RData')

library(ROCR)
calcRMSE <- function(label, estimation){
  return(sqrt(mean((label - estimation) ** 2)))
}
calcR2 <- function(label, estimation){
  RSS = sum((label - estimation) ** 2)
  SStot = sum((label - mean(label)) ** 2)
  return(1-RSS/SStot)
}
getRecall <- function(i) {
  conf.credit.train <- table(pred=credit_train$pred>i, actual=credit_train$default.payment.next.month)
  conf.credit.train[2,2]/sum(conf.credit.train[,2])
}
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

# Part 1
## 1
student_model <- lm(G3 ~ ., student.train)
student_model
summary(student_model)

## 2
student.train$pred <- predict(student_model, newdata = student.train)
student.test.nolabel$pred <- predict(student_model, newdata = student.test.nolabel)
calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)

## 4
student_model <- lm(G3 ~ . + I(studytime^2)+I(health^2), student.train)
student.train$pred <- predict(student_model, newdata = student.train)

calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)

## 5
student_model <- lm(G3 ~ school + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob + reason
                    + guardian + traveltime + studytime + failures + schoolsup + famsup + paid + nursery
                    + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health
                    + absences + class, student.train)

student.train$pred <- predict(student_model, newdata = student.train)

calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)

## 6
student_model <- lm(G3 ~ . + I(studytime^2)+I(health^2), student.train)
student.train$pred <- predict(student_model, newdata = student.train)
student.test.nolabel$pred <- predict(student_model, newdata = student.test.nolabel)
pred_grade_test <- student.test.nolabel$pred
save(pred_grade_test, file="st21400022.RData")

# 2
## 1
fmla <- "default.payment.next.month~."
credit_model <- glm(fmla, data=credit_train, family = binomial(link='logit'))
credit_model
summary(credit_model)
coefficients(credit_model)
credit_train$pred <- predict(credit_model, newdata = credit_train, type="response")
credit_test$pred <- predict(credit_model, newdata = credit_test, type="response")

## 2
calAUC(credit_train$pred, credit_train$default.payment.next.month)

## 3
threshold <- 0.5

conf.credit.train <- table(pred=credit_train$pred>threshold, actual=credit_train$default.payment.next.month)
credit_train_prec <- conf.credit.train[2,2]/sum(conf.credit.train[2,])
credit_train_prec
credit_train_rec <- conf.credit.train[2,2]/sum(conf.credit.train[,2])
credit_train_rec
credit_train_acc <- sum(diag(conf.credit.train)) / sum(conf.credit.train)
credit_train_acc

thres_list <- seq(0.01, 0.50, 0.01)
rec_list <- sapply(thres_list, getRecall)
plot(x=thres_list, rec_list, xlab="Threshold", ylab="Recall", type="l")

thres_list <- 0.01

conf.credit.train <- table(pred=credit_train$pred>threshold, actual=credit_train$default.payment.next.month)
credit_train_prec <- conf.credit.train[2,2]/sum(conf.credit.train[2,])
credit_train_prec
credit_train_rec <- conf.credit.train[2,2]/sum(conf.credit.train[,2])
credit_train_rec
credit_train_acc <- sum(diag(conf.credit.train)) / sum(conf.credit.train)
credit_train_acc

## 4
fmla <- "default.payment.next.month~.+I(AGE^2)+I(EDUCATION^2)+I(MARRIAGE^2)+I(PAY_AMT2^2)+I(PAY_AMT3^2)"
credit_model <- glm(fmla, data=credit_train, family = binomial(link='logit'))

credit_train$pred <- predict(credit_model, newdata = credit_train, type="response")
credit_test$pred <- predict(credit_model, newdata = credit_test, type="response")

calAUC(credit_train$pred, credit_train$default.payment.next.month)

conf.credit.train <- table(pred=credit_train$pred>threshold, actual=credit_train$default.payment.next.month)
credit_train_prec <- conf.credit.train[2,2]/sum(conf.credit.train[2,])
credit_train_prec
credit_train_rec <- conf.credit.train[2,2]/sum(conf.credit.train[,2])
credit_train_rec
credit_train_acc <- sum(diag(conf.credit.train)) / sum(conf.credit.train)
credit_train_acc

## 5
prob_default_test <- credit_test$pred
pred_default_test <- credit_test$pred > threshold
save(prob_default_test, pred_grade_test, pred_default_test, file="st21400022.RData")