## Final

# 1

load('final_test.RData')

library(ROCR)
calcRMSE <- function(label, estimation){
  return(sqrt(mean((label - estimation) ** 2)))
}
calcR2 <- function(label, estimation){
  RSS = sum((label - estimation) ** 2)
  SStot = sum((label - mean(label)) ** 2)
  return(1-RSS/SStot)
}
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

fmla <- "Revenue~."
shopping_model <- glm(fmla, data = shopping_train, family = binomial(link='logit'))
shopping_model
summary(shopping_model)
coefficients(shopping_model)
shopping_train$pred <- predict(shopping_model, newdata = shopping_train, type="response")
shopping_test$pred <- predict(shopping_model, newdata = shopping_test, type="response")

threshold <- 0.5
conf.shopping.train <- 
  table(pred=shopping_train$pred>threshold, actual=shopping_train$Revenue)
shopping_train_prec <- conf.shopping.train[2,2]/sum(conf.shopping.train[2,])
shopping_train_rec <- conf.shopping.train[2,2]/sum(conf.shopping.train[,2])
shopping_train_acc <- (conf.shopping.train[1,1] / (conf.shopping.train[1,1] + conf.shopping.train[1,2])
                       + conf.shopping.train[2,2] / (conf.shopping.train[2,1] + conf.shopping.train[2,2])) / 2
shopping_train_prec
shopping_train_rec
shopping_train_acc
calAUC(shopping_train$pred, shopping_train$Revenue)

## above are precision for all data 0.7527
fmla <- "Revenue~.+I(ExitRates^2)"
shopping_model <- glm(fmla, data = shopping_train, family = binomial(link='logit'))
shopping_train$pred <- predict(shopping_model, newdata = shopping_train, type="response")
shopping_test$pred <- predict(shopping_model, newdata = shopping_test, type="response")

# Repeat process and get lower precision, so not add var

fmla <- "Revenue~.+I(ExitRates^2)+I(PageValues^2)"
shopping_model <- glm(fmla, data = shopping_train, family = binomial(link='logit'))
shopping_train$pred <- predict(shopping_model, newdata = shopping_train, type="response")
shopping_test$pred <- predict(shopping_model, newdata = shopping_test, type="response")

threshold <- 0.2
conf.shopping.train <- 
  table(pred=shopping_train$pred>threshold, actual=shopping_train$Revenue)
shopping_train_prec <- conf.shopping.train[2,2]/sum(conf.shopping.train[2,])
shopping_train_rec <- conf.shopping.train[2,2]/sum(conf.shopping.train[,2])
shopping_train_acc <- (conf.shopping.train[1,1] / (conf.shopping.train[1,1] + conf.shopping.train[1,2])
                       + conf.shopping.train[2,2] / (conf.shopping.train[2,1] + conf.shopping.train[2,2])) / 2
shopping_train_prec
shopping_train_rec
shopping_train_acc
calAUC(shopping_train$pred, shopping_train$Revenue)

prob_shopping_test <- shopping_test$pred
pred_shopping_test <- shopping_test$pred > threshold
save(pred_housing_test, prob_shopping_test, pred_shopping_test, file="st21400022.RData")

# 2

library(tidyr)
library(stringr)
housing_train$X1 <- as.numeric(housing_train$X1)
housing_train$X1 <- as.character(housing_train$X1)
housing_train <- separate(housing_train, X1, c("Year", "Month"))
housing_train$Month <- ifelse(is.na(housing_train$Month), 0, housing_train$Month)
housing_train$Month <- str_pad(housing_train$Month, width=3, side="right", pad="0")
housing_train$Month <- as.numeric(housing_train$Month)
housing_train$Year <- as.numeric(housing_train$Year)


housing_test$X1 <- as.numeric(housing_test$X1)
housing_test$X1 <- as.character(housing_test$X1)
housing_test <- separate(housing_test, X1, c("Year", "Month"))
housing_test$Month <- ifelse(is.na(housing_test$Month), 0, housing_test$Month)
housing_test$Month <- str_pad(housing_test$Month, width=3, side="right", pad="0")
housing_test$Month <- as.numeric(housing_test$Month)
housing_test$Year <- as.numeric(housing_test$Year)


housing_model <- lm(Y ~ ., housing_train)
housing_model
summary(housing_model)
coefficients(housing_model)

housing_train$pred <- predict(housing_model, newdata = housing_train)
housing_test$pred <- predict(housing_model, newdata = housing_test)

calcRMSE(housing_train$Y, housing_train$pred)
calcR2(housing_train$Y, housing_train$pred)
# 8.197366 0.6099

## delete few var
housing_model <- lm(Y ~ Year+Month+X2+X3+X4+X5+X6, housing_train)
# get lower

## add new var
housing_model <- lm(Y ~ .+I(Month^2)+I(X2^2)+I(X3^2)+I(X4^2)+I(X5^2)
                    +I(Year^2)+I(Month^2)+I(X6^2)
                    +Month:X2+Month:X3+Month:X6
                    +X2:X4+X2:X5+X2:X6+X2:X3
                    +Year:Month+Year:X2+Year:X3+Year:X4+Year:X6
                    +X3:X4+X3:X5+X3:X6
                    +X4:X5+X4:X6, housing_train)
housing_train$pred <- predict(housing_model, newdata = housing_train)
housing_test$pred <- predict(housing_model, newdata = housing_test)

calcRMSE(housing_train$Y, housing_train$pred)
calcR2(housing_train$Y, housing_train$pred)

# 6.396 0.7625

pred_housing_test <- housing_test$pred
save(pred_housing_test, prob_shopping_test, pred_shopping_test, file="st21400022.RData")
