traindata = read.csv(file.choose())
View(traindata)
attach(traindata)
traindata = traindata[-c(1,14)]

summary(traindata)
str(traindata)
cor(traindata)
is.na(traindata)
sum(is.na(traindata))

testdata = read.csv(file.choose())
View(testdata)
attach(testdata)
testdata = testdata[-c(1,13)]

summary(testdata)
str(testdata)
cor(testdata)
is.na(testdata)
sum(is.na(testdata))

## Considering only 10k values because of huge dataset
train1=traindata[1:10000,]
pairs(train1)


#### Multilinear Regression
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(traindata))


model1 = lm(pm~. , data = train1)
summary(model1)


#### R squared value we got 96.23 % and adjusted R squared is 96.23%
### For all the input variables except "coolant","u_d" and "u_q" we got less than 0.05 probability

model.coolant = lm(pm~coolant ,data=train1)
summary(model.coolant) ## Coolant became significant


model.ud = lm(pm~u_d ,data=train1)
summary(model.ud) ## u_d became significant
model.ud$residuals

model.uq = lm(pm~u_q ,data=train1)
summary(model.uq) ## u_q became significant
model.uq$residuals


model.cdq = lm(pm~coolant+u_d+u_q ,data=train1)
summary(model.cdq) ## Coolant became insignificant
model.cdq$residuals


influence.measures(model.cdq)
install.packages("car")
library(car)
vif(model.cdq) ## Original model

avPlots(model.cdq,id.n=2,id.cex=0.7)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor)


pairs(train1,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")
influence.measures(model.cdq)
library(car)
influenceIndexPlot(model.cdq,id.n=3) ##index plots for influence measures
influencePlot(model.cdq, id.n=3)  ## A user friendly representation of the above

model_1 = lm(pm~. -coolant,data=train1[-1,]) ##1
summary(model_1)
model_1$residuals
plot(lm(pm~. -coolant,data = train1[-1,]))




model_2 = lm(pm~. -u_d,data=train1[-c(2,5),]) ##2, 5
summary(model_2)
plot(lm(pm~. -u_d,data=train1[-c(2,5),]))

model_3 = lm(pm~. -u_d,data=train1[-c(2,5,4583,4584),]) ##2,5,4583,4584
summary(model_3)
## u_d is mostly influencing permanent magnet
## So we neglected the outliers in u_d variable


##Final Mode
finalmodel = lm(pm~. -u_d,data=train1[-c(2,5,4583,4584),]) ##2,5,4583,4584
summary(finalmodel)
plot(finalmodel)
hist(residuals(finalmodel))
#### R squared value we got 96.23 % and adjusted R squared is 96.23%
### RMSE values
library(caret)
install.packages("iterators")
library(iterators)
data(traindata)
control<-trainControl(method = "cv",number = 12)
set.seed(7)


fit<-train(pm~., data=traindata, method="lm", metric="RMSE", trControl=control)
print(fit)
#### RMSE, Rsquared and MAE are 0.4749, 0.7723, 0.3617
pred<-predict(fit,newdata = testdata)
pred
summary(pred)

## Considering only 15k values because of huge dataset
train2=traindata[10001:25000,]
pairs(train2)


#### Multilinear Regression
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(train2))


model1 = lm(pm~. , data = train2)
summary(model1)


#### R squared value we got 92.08 % and adjusted R squared is 92.08%
### For all the input variables except "i_d" we got less than 0.05 probability

model.coolant = lm(pm~coolant ,data=train2)
summary(model.coolant) ## u_d became significant
model.coolant$residuals


model.id = lm(pm~i_d ,data=train2)
summary(model.id) ## u_d became significant
model.id$residuals

model.cid = lm(pm~coolant+i_d ,data=train2)
summary(model.cid) ## u_d became significant
model.cid$residuals



influence.measures(model.cid)
install.packages("car")
library(car)
vif(model.cid) ## Original model

avPlots(model.cid,id.n=2,id.cex=0.7)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
  
  
pairs(train2,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")
influence.measures(model.cid)
library(car)
influenceIndexPlot(model.cid,id.n=3) ##index plots for influence measures
influencePlot(model.cid, id.n=3)  ## A user friendly representation of the above

model_1 = lm(pm~. -coolant,data=train2[-23320,]) ##23320
summary(model_1)
model_1$residuals
plot(lm(pm~. -coolant,data = train2[-23320,]))


model_2 = lm(pm~. -i_d,data=train2[-c(23321,23323),]) ##23321, 23322
summary(model_2)
plot(lm(pm~. -i_d,data=train2[-c(23321,23323),]))

model_3 = lm(pm~. -i_d,data=train2[-c(24434),]) ##24434
summary(model_3)
plot(lm(pm~. -i_d,data=train2[-c(24434),]))
## i_d is mostly influencing permanent magnet
## So we neglected the outliers in i_d variable


##Final Mode
finalmodel = lm(pm~. -i_d,data=train2[-c(23321,23323,24434),]) ##23321,23323,24434
summary(finalmodel)
plot(finalmodel)
hist(residuals(finalmodel))
#### R squared value we got 92.08 % and adjusted R squared is 92.08%

## Considering only 15k values because of huge test dataset
test1=testdata[1:15000,]

predictmodel1 <- predict(finalmodel , newdata = test1)
predictmodel1

rmse1 <- sqrt(mean((predictmodel1-train2$motor_speed)^2))
rmse1  # 1.495

Adjusted_RSqred <- function(pred, obs, formula = "corr", na.rm = FALSE)
{
  n <- sum(complete.cases(predictmodel1))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

Adjusted_RSqred(predictmodel1, train2$motor_speed) # 0.9536684






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















## Number of nodes of trees

hist(treesize(fit.forest), main = "No of Nodes for the trees", col= "green")
## Majority of the trees has an average number of more than 50 nodes

## variable importance
varImpPlot(fit.forest)

## Quantitative values
importance(fit.forest)

varUsed(fit.forest) ## which predictor variables are actually used in randomforest

## Partial dependence plot
partialPlot(fit.forest, train1,motor_speed, "Positive")

### On that graph, i see that if motor speed is greater than 0 then we can
### assume that motor heating is in good condition


### KNN Technique

traindata = read.csv(file.choose())
View(traindata)
attach(traindata)
traindata = traindata[-1]

install.packages('caTools')  ##for train and test data split
install.packages('dplyr')    ##for Data Manipulation
install.packages('ggplot2')  ##for Data Visualization
install.packages('class')    ##KNN 
install.packages('caret')    ##Confusion Matrix
install.packages('corrplot') ##Correlation Plot
library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)

## table on different types of profile_ids 
table(traindata$profile_id)
traindata$profile_id <- as.factor(traindata$profile_id)
str(traindata)

## Table or proportation of enteries in the datasets. 
## What % of glass of Type 1 and what % of glass of Type 2
round(prop.table(table(traindata$profile_id))*100,1)
summary(traindata[c("ambient","coolant", "u_d","u_q","motor_speed","torque","i_d")])

## Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
## Test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))



train_n<-traindata[1:12]
train_n<-cbind(traindata$profile_id,train_n[1:12])
View(train_n)


## Create training and test datasets
train <- train_n[1:50000,]
test <- train_n[50001:70000,] 

## Get labels for training and test datasets
train_labels <- train[1:50000,1]
test_labels <- test[50001:70000,1]

## Build a KNN model on taining dataset
library("class")
library(caret)
## Building the KNN model on training dataset and also need labels which we are including c1. 
## Once we build the prediction model
## We have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(7,100,2))
{
  train_pred <- knn(train=train,test=train,cl=train_labels,k=i)
  train_acc <- c(train_acc,mean(train_pred==train_labels))
  test_pred <- knn(train = train, test = test, cl = train_labels, k=i)
  test_acc <- c(test_acc,mean(test_pred==test_labels))
}


## Testing Accuracy 

## Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,100,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,100,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(7,100,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))



pred <- knn(train = train, test = test, cl = train_labels, k=21)
class(train)
class(test)
library(gmodels)
# Create cross table of predicted and actual
CrossTable(x=test_labels, y=pred)



#principal component analysis 

cor(sample_train1)
cor(samp_test1)

pca_train1 <- princomp(sample_train1 , cor = TRUE , covmat = NULL , scores = TRUE)
summary(pca_train1)
str(pca_train1)
loadings(pca_train1)
plot(pca_train1)
biplot(pca_train1)
plot(cumsum(pca_train1$sdev*pca_train1$sdev)*100/(sum(pca_train1$sdev*pca_train1$sdev)),type = "b")
pca1 <- pca_train1$scores[,1:5]
summary(pca1)

pca_test1 <- princomp(samp_test1 , cor = TRUE , covmat = NULL , scores = TRUE)
summary(pca_test1)
str(pca_test1)
loadings(pca_test1)
plot(pca_test1)
biplot(pca_test1)
plot(cumsum(pca_test1$sdev*pca_test1$sdev)*100/(sum(pca_test1$sdev*pca_test1$sdev)),type = "b")
pca2 <- pca_test1$scores[,1:5]
summary(pca2)

#applying the clustering analysis 

norm_samp_train1 <- scale(sample_train1)

library(plyr)

x <- runif(50)
y <- runif(50)

data1 <- cbind(x,y)
View(data1)

kmeans_train1 <- kmeans(data1,5)
str(kmeans_train1)
kmeans_train1$cluster
kmeans_train1$withinss
kmeans_train1$centers

km <- kmeans.ani(data1,5)

wss =  (nrow(norm_samp_train1)-1)*sum(apply(norm_samp_train1, 2, var))#to find the elbow curve
for (i in 1:3) wss[i] = sum(kmeans(norm_samp_train1, centers=i)$withinss)
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
title(sub = "K-Means Clustering Scree-Plot")

#building the model 

fit <- kmeans(norm_samp_train1,2)
fit$cluster
final1 <- data.frame(sample_train1,fit$cluster)
View(final1)
fit$size    #22117 27883

