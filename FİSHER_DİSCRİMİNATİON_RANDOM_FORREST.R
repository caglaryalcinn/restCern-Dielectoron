
#we desire to discrimant analys on electrons sign stitation(dielectron or mass).we checked fisher discrimination analysis assumtion before.so we can continue,Firstly we subset desire numeriv variables and we separate to data as tran an test.

data8 <- data5[,c(3,5,6,7,8,9,11,13,15,17,20)]
sample <- sample(c(TRUE, FALSE), nrow(data8), replace=TRUE, prob=c(0.7,0.3))
train <- data8[sample, ]
test <- data8[!sample, ] 

#after seperation we conduct the model.plot of model shows the linear discriminants obtained from the equation .And we conduct the predict model values."predict(Model)" return 3 variable and one of them "x" that we intrest in.


library(MASS)
model <- lda(Sign~.,data = train)
plot(model)
model.values <- predict(model)


#and we obtain test prediction and train prediction values and we obtain their table .Finally we conduct to accuraccy of discrimination on train and test subsets that is 0.605 and 0.546 respectively.

train_predict<- predict(model,train)$class
test.predict <- predict(model,test)$class
table_train <- table(Predicted =train_predict, Actual = train$Sign)
table_train
table_test <- table(Predicted =test.predict, Actual = test$Sign)
table_test
sum(diag(table_train))/sum(table_train)
sum(diag(table_test))/sum(table_test)


########### construct random forrest model,there is no assumtion normality in RF so we use orjinal data.
data9 <- data5orj[,c(3,5,6,7,8,9,11,13,15,17,20)]
sample2 <- sample(c(TRUE, FALSE), nrow(data9), replace=TRUE, prob=c(0.7,0.3))
train2 <- data9[sample, ]
train
test2 <- data9[!sample, ] 
data5orj$Sign<-as.factor(data5orj$Sign)
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train2[-5],
                             y = train2$Sign,
                             ntree = 1)
train2$Sign <- as.factor(train2$Sign)
test2$Sign <- as.factor(test2$Sign)
y_pred = predict(classifier_RF, newdata = test2[-5])
y_pred.train = predict(classifier_RF, newdata = train2[-5])
table_test2 <- table(Predicted =y_pred, Actual = test2$Sign)
table_train2 <- table(Predicted =y_pred.train, Actual = train2$Sign)
table_train2
table_test2
sum(diag(table_train2))/sum(table_train2)
sum(diag(table_test2))/sum(table_test2)
plot(classifier_RF)
importance(classifier_RF)
var<-varImpPlot(classifier_RF)
var
