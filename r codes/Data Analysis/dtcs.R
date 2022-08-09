getwd()
setwd("C:/Users/sruth/OneDrive/Desktop/CSE4027 LAB")
dir()
da=read.csv("customer_segmentation_cleaned.csv")
da


#data modelling
library(caTools)
set.seed(100)
data_sample=sample.split(da$Gender,SplitRatio=0.80)
train_data = subset(da,data_sample==TRUE)
test_data = subset(da,data_sample==FALSE)
dim(train_data)
dim(test_data)

#decision tree
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(as.factor(Gender) ~ . , da, method = 'class')
predicted_val <- predict(decisionTree_model, da, type = 'class')
probability <- predict(decisionTree_model, da, type = 'prob')
rpart.plot(decisionTree_model)

#building the confusion matrix 
tp<-predict(decisionTree_model,test_data,type = "class")
tp

tr<-predict(decisionTree_model,train_data,type = "class")
tr

#Building the confusion matrix for training data
cm<-table(Actual_Value=train_data$Gender,Predicted_Value= tr)
cm

#Accuracy
accuracy=(cm[[1,1]]+cm[[2,2]])/sum(cm)
accuracy

#Building  the confusion matrix for testing Data
cmte<-table(Actual_Value=test_data$Gender,Predicted_Value= tp)
cmte

#accuracy
accuracy=(cmte[[1,1]]+cmte[[2,2]])/sum(cmte)
accuracy
