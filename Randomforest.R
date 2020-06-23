### Random Forest
install.packages("randomForest")
library(randomForest)
library(caret)


## From the summary we have motor speed ranges from "-1.3" to "2.2"
## Hence we are treating motor speed less than "0" as negative and other's positive

hist(traindata$pm)## motor speed ranges from "-1.3" to "2.2"
hist(traindata$pm, main = "Range of pm", xlim = c(-1.3,2.2),
     breaks = c(seq(20000,60000,100000)), col= c("blue","red","green","violet"))

Negative_positive = ifelse(traindata$pm <= 0, "Negative", "Positive")
View(Negative_positive)
NPtrain = data.frame(traindata, Negative_positive)

str(NPtrain)

table(NPtrain$Negative_positive)
### Less than "0" values are 3,18,890 
### Greater than "0" values are 3,79,759


## Splitting data into training and testing as the Negative_positive are in order
## Spitting the data based on negative_positive

train1 = NPtrain[1:100000,] ## splitting the train data from 1 to 100000
##View(train1)
test1 = NPtrain[100001:150000,] ## splitting the test data from 100001 to 150000
##View(test1)

## Building a random forest model on train data
fit.forest = randomForest(Negative_positive~. , data= train1, 
                          na.action = na.roughfix,importance= TRUE)

fit.forest
## Out of bag estimate of error rate is 0% in Random Forest model
## because of less number of data
attributes(fit.forest)
plot(fit.forest)
legend("topright", colnames(fit.forest$err.rate), col = 1:3, cex=0.8, fill=1:3)

## Prediction and confusion matrix - Training Data
pred1 = predict(fit.forest,train1)
head(pred1)
head(train1$Negative_positive)
## Looks like first 6 predicted values and original values matches


library(e1071)
confusionMatrix(pred1,train1$Negative_positive) ## 100% accuracy on train data


## Prediction with test data
pred2= predict(fit.forest, test1)
head(pred2)
head(test1$Negative_positive)
## Looks like first 6 predicted values and original values matches
confusionMatrix(pred2,test1$Negative_positive)


## Again Splitting data into training and testing as the Negative_positive are in order
## Spitting the data based on negative_positive

train2 = NPtrain[100001:200000,] ## splitting the train data from 100001 to 200000
##View(train1)
test2 = NPtrain[150001:200000,] ## splitting the test data from 150001 to 200000
##View(test1)

## Building a random forest model on train data
fit.forest1 = randomForest(Negative_positive~. , data= train2, 
                           na.action = na.roughfix,importance= TRUE)

fit.forest1
## Out of bag estimate of error rate is 0% in Random Forest model
## because of less number of data
attributes(fit.forest1)
plot(fit.forest1)
legend("topright", colnames(fit.forest1$err.rate), col = 1:3, cex=0.8, fill=1:3)

## Prediction and confusion matrix - Training Data
pred3 = predict(fit.forest1,train2)
head(pred3)
head(train2$Negative_positive)
## Looks like first 6 predicted values and original values matches


library(e1071)
confusionMatrix(pred3,train2$Negative_positive) ## 100% accuracy on train data


## Prediction with test data
pred4= predict(fit.forest1, test2)
head(pred4)
head(test2$Negative_positive)
## Looks like first 6 predicted values and original values matches
confusionMatrix(pred4,test2$Negative_positive)
