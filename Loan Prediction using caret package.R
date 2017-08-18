path="/Users/abhilekh/Downloads/"
setwd(path)
install.packages("caret", dependencies = c("Depends","Suggests"))
install.packages("RANN")
library(caret)
library(RANN)
train<-read.csv("train_u6lujuX_CVtuZ9i.csv",stringsAsFactors = T)
test<-read.csv("test_Y3wMUE5_7gLdaTN.csv")
str(train)

#1. Data pre processing
#check for missing values
sum(is.na(train)) #86 missing values(i.e. NA's found)

#use knn to impute these missing values
#scale and centre the numerical columns using preProcess() in caret

preprocValues<-preProcess(train,method = c("knnImpute","center","scale"))
train_processed<-predict(preprocValues,train)
sum(is.na(train_processed))
#convert loan status to numeric
train_processed$Loan_Status<-ifelse(train_processed$Loan_Status=="N",0,1)
train_processed$Loan_ID=as.numeric(train_processed$Loan_ID)

#create dummy vars for categorical vars using one hot encoding
dmy<-dummyVars("~.", data=train_processed,fullRank = T)
train_transformed<-data.frame(predict(dmy,newdata=train_processed))
str(train_transformed)
train_transformed$Loan_Status=as.factor(train_transformed$Loan_Status)
#splitting train set into two parts for cross validation 75% and 25%
index<-createDataPartition(train_transformed$Loan_Status,p=0.75,list = FALSE)
trainSet<-train_transformed[index,]
testSet<-train_transformed[-index,]

#Features selection using Recursive feature elimination in caret
control<-rfeControl(functions = rfFuncs, method="repeatedcv", repeats = 3,verbose = FALSE)
predictors<-setdiff(names(trainSet),"Loan_Status")
Loan_Pred_Profile<-rfe(trainSet[,predictors], trainSet[,"Loan_Status"],rfeControl = control)

names(getModelInfo())#gives list of all ML algo's in caret package
