### MMA 867 Individual Assignment - Tiffany Zeng
### Kaggle Competitiion - Bike Sharing Demand

library(tidyr)
library(glmnet)

train <- read.csv(file.choose(), header=TRUE, sep=",")
test <- read.csv(file.choose(), header=TRUE, sep=",")

summary(train)
summary(test)

train <- train %>% separate(datetime, c("year", "month", "day", "time"))
train$season <- ifelse(train$season == 1, "spring", train$season)
train$season <- ifelse(train$season == 2, "summer", train$season)
train$season <- ifelse(train$season == 3, "fall", train$season)
train$season <- ifelse(train$season == 4, "winter", train$season)
train$weather <- ifelse(train$weather == 1, "clear", train$season)
train$weather <- ifelse(train$weather == 2, "mist", train$season)
train$weather <- ifelse(train$weather == 3, "snow", train$season)
train$weather <- ifelse(train$weather == 4, "rain", train$season)
set.seed(1)
rows <- sample(nrow(train))
train <- train[rows, ]
train$ID <- seq.int(nrow(train))

test$datetime.copy <- test$datetime
test <- test %>% separate(datetime, c("year", "month", "day", "time"))
test$season <- ifelse(test$season == 1, "spring", test$season)
test$season <- ifelse(test$season == 2, "summer", test$season)
test$season <- ifelse(test$season == 3, "fall", test$season)
test$season <- ifelse(test$season == 4, "winter", test$season)
test$weather <- ifelse(test$weather == 1, "clear", test$season)
test$weather <- ifelse(test$weather == 2, "mist", test$season)
test$weather <- ifelse(test$weather == 3, "snow", test$season)
test$weather <- ifelse(test$weather == 4, "rain", test$season)

training <- subset(train, ID<=8000)
testing <- subset(train, ID>8000)

fit1 <- lm(count~year+month+time+season+holiday+workingday+weather+temp+atemp+humidity+windspeed, training)
predicted.count.testing1 <- predict(fit1, testing)
percent.errors1 <- abs((testing$count-predicted.count.testing1)/testing$count)*100
mean(percent.errors1) #270.5722
par(mfrow=c(2,2))
plot(fit1)
plot(count~temp, data=train)
plot(count~atemp, data=train)

plot(predicted.count.testing1 ~ testing$temp) 
abline(0,0)
plot(predicted.count.testing1 ~ testing$atemp)
abline(0,0)

fit2 <- lm(log(count)~year+month+time+season+holiday+workingday+weather+log(temp)+log(atemp)+humidity+windspeed, training)
predicted.count.testing2 <- exp(predict(fit2, testing))
percent.errors2 <- abs((testing$count-predicted.count.testing2)/testing$count)*100
mean(percent.errors2) #58.39214

y <- log(training$count)
X.train <- model.matrix(ID~year+month+time+season+season*weather+holiday+holiday*time*month*season+workingday+workingday*time*month*season+weather+
                        log(temp)*weather*humidity*windspeed*season+atemp*weather*humidity*windspeed*season+humidity+windspeed, train)[,-1]
X.train <- cbind(train$ID,X.train)

X.test <- model.matrix(datetime.copy~year+month+time+season+season*weather+holiday+holiday*time*month*season+workingday+workingday*time*month*season+
                       weather+log(temp)*weather*humidity*windspeed*season+atemp*weather*humidity*windspeed*season+humidity+windspeed, test)[,-1]
X.test <- cbind(test$datetime.copy,X.test)

X.training <- subset(X.train, X.train[,1]<=8000)
X.testing <- subset(X.train, X.train[,1]>8000)

lasso.fit <- glmnet(x = X.training, y = y, alpha = 1)
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1)
penalty.lasso <- crossval$lambda.min
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs((testing$count-lasso.testing)/testing$count)*100)

ridge.fit <- glmnet(x = X.training, y = y, alpha = 0)
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
penalty.ridge <- crossval$lambda.min
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs((testing$count-ridge.testing)/testing$count)*100)

predicted.count <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.test))
test$count <- predicted.count
write.csv(test, file = "Predicted Count.csv")