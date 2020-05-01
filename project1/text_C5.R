
#written by Francisco Baca

#TESTING ALTERNATIVE MODEL C5 ON THE DATA
rm(list = ls())
gc()

###INCOMPLETE###
library(magrittr) #for using pipes (more concise code, but slightly less explicit)
#text mining libraries
library(dplyr)
library(caret)  #for pre-processing
library(C50)
#library(kernlab)
library(data.table) #for memory efficiency
#library(doParallel)

#performing C5 on description attribute
cat("performing C5 on description attribute\n\n")
train_dtm <- fread("./text_data/description_train_DTM.csv")
test_dtm <- fread("./text_data/description_test_DTM.csv")
mc <- table(test_dtm$fraudulent)[2]
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
train_dtm$fraudulent %<>% factor
test_dtm$fraudulent %<>% factor
C5 <- C5.0(fraudulent~., data=train_dtm, control = C5.0Control(minCases=mc))
gc()
#testing prediction performance on training data with model trained with training data
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, train_dtm)
description_train_train_cm <- confusionMatrix(predictions, train_dtm$fraudulent, positive='1')
print(description_train_train_cm)
cat("\n\n")
gc()
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, test_dtm)
description_train_test_cm <- confusionMatrix(predictions, test_dtm$fraudulent, positive='1')
print(description_train_test_cm)
cat("\n\n\n")
gc()


#performing C5 on requirements attribute
cat("performing C5 on requirements attribute\n\n")
train_dtm <- fread("./text_data/requirements_train_DTM.csv")
test_dtm <- fread("./text_data/requirements_test_DTM.csv")
mc <- table(test_dtm$fraudulent)[2]
train_dtm$fraudulent %<>% factor
test_dtm$fraudulent %<>% factor
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
C5 <- C5.0(fraudulent~., data=train_dtm, control = C5.0Control(minCases=mc))
gc()
#testing prediction performance on training data with model trained with training data
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, train_dtm)
requirements_train_train_cm <- confusionMatrix(predictions, train_dtm$fraudulent, positive='1')
print(requirements_train_train_cm)
cat("\n\n")
gc()
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, test_dtm)
requirements_train_test_cm <- confusionMatrix(predictions, test_dtm$fraudulent, positive='1')
print(requirements_train_test_cm)
cat("\n\n\n")
gc()


#performing C5 on benefits attribute
cat("performing C5 on benefits attribute\n\n")
train_dtm <- fread("./text_data/benefits_train_DTM.csv")
test_dtm <- fread("./text_data/benefits_test_DTM.csv")
mc <- table(test_dtm$fraudulent)[2]
train_dtm$fraudulent %<>% factor
test_dtm$fraudulent %<>% factor
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
C5 <- C5.0(fraudulent~., data=train_dtm, control = C5.0Control(minCases=mc))
gc()
#testing prediction performance on training data with model trained with training data
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, train_dtm)
benefits_train_train_cm <- confusionMatrix(predictions, train_dtm$fraudulent, positive='1')
print(benefits_train_train_cm)
cat("\n\n")
gc()
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, test_dtm)
benefits_train_test_cm <- confusionMatrix(predictions, test_dtm$fraudulent, positive='1')
print(benefits_train_test_cm)
cat("\n\n\n")
gc()


#performing C5 on company_profile attribute
cat("performing C5 on company_profile attribute\n\n")
train_dtm <- fread("./text_data/company_profiles_train_DTM.csv")
test_dtm <- fread("./text_data/company_profiles_test_DTM.csv")
mc <- table(test_dtm$fraudulent)[2]
train_dtm$fraudulent %<>% factor
test_dtm$fraudulent %<>% factor
#finding the column name intersection between training and testing partitions
#so the terms match, (partitions are still different and valid), just making sure
#the fields match and anything extra gets discarded 
intersect_cols <- intersect(colnames(train_dtm), colnames(test_dtm))
train_dtm <- as.data.table(train_dtm[, ..intersect_cols])
test_dtm <- as.data.table(test_dtm[, ..intersect_cols])
gc()
C5 <- C5.0(fraudulent~., data=train_dtm, control = C5.0Control(minCases=mc))
gc()
#testing prediction performance on training data with model trained with training data
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, train_dtm)
company_profiles_train_train_cm <- confusionMatrix(predictions, train_dtm$fraudulent, positive='1')
print(company_profiles_train_train_cm)
cat("\n\n")
gc()
cat("testing prediction performance on training data with model trained with training data\n\n")
predictions <- predict(C5, test_dtm)
company_profiles_train_test_cm <- confusionMatrix(predictions, test_dtm$fraudulent, positive='1')
print(company_profiles_train_test_cm)
cat("\n\n\n")
gc()
