# Linear Regression on Suburb Boston Housing Dataset
#Last Modified: 12th March '16

#Clear workspace
rm(list = ls(all.names = TRUE))

#Clear console
cat("\014")

#load the MASS library for using 'ginv' function
require(MASS)

#For description of data refer to this URL: https://archive.ics.uci.edu/ml/datasets/Housing
#Read Suburb Boston housing data-set into the 'Boston' dataframe
Boston<-read.csv("../DATA/Housing_Data_for_Suburb_Boston.csv")

#Add a column of 1s to account for the w_0 term in w
Boston<-cbind(rep(1,nrow(Boston)),Boston)

#Split data into training set and testing set
train<-Boston[1:253,1:ncol(Boston)-1]
test<-Boston[254:nrow(Boston),1:ncol(Boston)-1]

#zn, chas columns have lots of zeros or repetitions of the same entry.
#We remove those columns for a good fit.
train$zn<-NULL; train$chas<-NULL; test$zn<-NULL; test$chas<-NULL

#Identify the linear model "train*w=target_var_train"
train<-as.matrix(train)
test<-as.matrix(test)
target_var_train<-as.matrix(Boston[1:253,"medv"])
target_var_test<-as.matrix(Boston[254:nrow(Boston),"medv"])

#Compute the coefficient vector 'w'
w<-ginv(train) %*% target_var_train

#Compute absolute error on the test set
absolute_error<-(test %*% w) - target_var_test

#Compute mean error on the test set
#which.max(absolute_error)
mean_error<-mean(absolute_error^2)
mean_error
