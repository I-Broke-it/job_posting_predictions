
#written by Francisco Baca

#KSVM ON THIS DATA REQUIRES A LOT OF MEMORY, 
#THIS SCRIPT CAN ONLY BE RUN ON THE R SHELL
#WITH THE COMMAND LINE OPTION --max-ppsize [arg]
#WHERE THE APPROPRIATE VALUE FOR arg VARIES DEPENDING
#ON YOUR SYSTEM'S MEMORY 
rm(list = ls())
gc()

###INCOMPLETE###
library(magrittr) #for using pipes (more concise code, but slightly less explicit)

#text mining libraries
library(tm)
library(dplyr)
library(caret)  #for pre-processing, and model tuning
library(e1071) #for svm 
library(kernlab) #for svm
library(data.table)




cat("performing KSVM on description attribute\n\n")
train_dtm <- fread("./text_data/description_train_DTM.csv")
test_dtm <- fread("./text_data/description_test_DTM.csv")
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
#KSVM hyperparameter sigma is tuned at .2 to avoid overfitting
trained_ksvm <- ksvm(fraudulent~., data=train_dtm, kernel="rbfdot", sigma=.2)
gc()
#testing prediction performance on training data with model trained with training data
cat("testing the model's prediction performance on training data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, train_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
description_train_train_cm <- confusionMatrix(factor(predictions), factor(train_dtm$fraudulent), positive = '1')
print(description_train_train_cm)
cat("\n\n")
#testing prediction performance on test data with model trained with training data
cat("testing the model's prediction performance on test data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, test_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
description_train_test_cm <- confusionMatrix(factor(predictions), factor(train_dtm$fraudulent), positive = '1')
print(description_train_test_cm)
cat("\n\n\n")



cat("performing KSVM on requirements attribute\n\n")
train_dtm <- fread("./text_data/requirements_train_DTM.csv")
test_dtm <- fread("./text_data/requirements_test_DTM.csv")
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
#KSVM hyperparameter sigma is tuned at .2 to avoid overfitting
trained_ksvm <- ksvm(fraudulent~., data=train_dtm, kernel="rbfdot", sigma=.2)
gc()
#testing prediction performance on training data with model trained with training data
cat("testing the model's prediction performance on training data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, train_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
requirements_train_train_cm <- confusionMatrix(factor(predictions), factor(train_dtm$fraudulent), positive = '1')
print(requirements_train_train_cm)
cat("\n\n")
#testing prediction performance on test data with model trained with training data
cat("testing the model's prediction performance on test data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, test_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
requirements_train_test_cm <- confusionMatrix(factor(predictions), factor(test_dtm$fraudulent), positive = '1')
print(requirements_train_test_cm)
cat("\n\n\n")



cat("performing KSVM on benefits attribute\n\n")
train_dtm <- fread("./text_data/benefits_train_DTM.csv")
test_dtm <- fread("./text_data/benefits_test_DTM.csv")
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
#KSVM hyperparameter sigma is tuned at .2 to avoid overfitting
trained_ksvm <- ksvm(fraudulent~., data=train_dtm, kernel="rbfdot", sigma=.2)
gc()
#testing prediction performance on training data with model trained with training data
cat("testing the model's prediction performance on training data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, train_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
benefits_train_train_cm <- confusionMatrix(factor(predictions), factor(train_dtm$fraudulent), positive = '1')
print(benefits_train_train_cm)
cat("\n\n")
#testing prediction performance on test data with model trained with training data
cat("testing the model's prediction performance on test data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, test_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
benefits_train_test_cm <- confusionMatrix(factor(predictions), factor(test_dtm$fraudulent), positive = '1')
print(benefits_train_test_cm)
cat("\n\n\n")


cat("performing KSVM on company_profile attribute\n\n")
train_dtm <- fread("./text_data/company_profiles_train_DTM.csv")
test_dtm <- fread("./text_data/company_profiles_test_DTM.csv")
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
#KSVM hyperparameter sigma is tuned at .2 to avoid overfitting
trained_ksvm <- ksvm(fraudulent~., data=train_dtm, kernel="rbfdot", sigma=.2)
gc()
#testing prediction performance on training data with model trained with training data
cat("testing the model's prediction performance on training data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, train_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
company_profiles_train_train_cm <- confusionMatrix(factor(predictions), factor(train_dtm$fraudulent), positive = '1')
print(company_profiles_train_train_cm)
cat("\n\n")
#testing prediction performance on test data with model trained with training data
cat("testing the model's prediction performance on test data with model trained with training data\n\n")
predictions <- predict(trained_ksvm, test_dtm)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
company_profiles_train_test_cm <- confusionMatrix(factor(predictions), factor(test_dtm$fraudulent), positive = '1')
print(company_profiles_train_test_cm)
cat("\n\n\n")

