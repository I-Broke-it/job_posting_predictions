
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
library(splitstackshape)

#removes whitespace, punctuation, stopwords,
# amongst other content that may negatively
#affect svm model performance
clean_corpus <- function(corpus){
  corpus %<>% tm_map(., removeNumbers)
  corpus %<>% tm_map(., stripWhitespace)
  corpus %<>% tm_map(., removePunctuation)
  corpus %<>% tm_map(., content_transformer(tolower))
  corpus %<>% tm_map(., removeWords, stopwords("en"))
  corpus %<>% tm_map(., content_transformer(stemDocument), language = "english")
  
  return(corpus)
}

#assuming train and test csv's exist
#and that they are properly balanced 
#and valid partitions
jp_train <- read.csv('jp_train.csv')
jp_test <- read.csv('jp_test.csv')

#which columns contain text data elligible for text mining?
head(jp_train)

#columns eligible for text mining are benefits, description, requirements and company_profiles


#create and clean a dataframe of corpora for the benefits feature content (training)
train.feature_corpus <- jp_train$description %<>% VectorSource %>% VCorpus %>% clean_corpus
#create and clean a dataframe of corpora for the benefits feature content (testing)
test.feature_corpus <- jp_test$description %>% VectorSource %>% VCorpus %>% clean_corpus
fraud_col <- jp_train$fraudulent
rm(jp_test)
rm(jp_train)
gc()
#create a document term matrix for the training partition for the description attribute
train_dtm <- train.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

#create a document term matrix for the testing partition for the description attribute
test_dtm <- test.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

train_df <- intersect(colnames(train_dtm), colnames(test_dtm)) %>% train_dtm[,.] %>% data.frame
train_df$corpus <- fraud_col
test_df <- intersect(colnames(test_dtm), colnames(train_dtm)) %>% test_dtm[,.] %>% data.frame
rm(train_dtm)
rm(test_dtm)
gc()
test_df$corpus <- c("temp")
gc()

#training ksvm model on training data
trained_ksvm <- ksvm(corpus~., data=train_df, kernel="rbfdot")
gc()
#predicting training data from trained model
predictions <- predict(trained_ksvm, train_df)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1)
confusion_matrix <- confusionMatrix(factor(predictions), factor(train_df$corpus), positive = '1')
print(confusion_matrix)

#testing trained model predictions on test data
predictions <- predict(trained_ksvm, test_df)
gc()

predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
pred_df <- as.data.frame(predictions);
rownames(pred_df) <- rownames(predictions)
jp_test <- read.csv('jp_test.csv')
confusion_matrix <- confusionMatrix(factor(predictions), factor(jp_test$fraudulent), positive = '1')
print(confusion_matrix)

gc()







#REPEAT THE SAME PROCESS FOR requirements ATTRIBUTE

train.feature_corpus <- jp_train$requirements %<>% VectorSource %>% VCorpus %>% clean_corpus
#create and clean a dataframe of corpora for the benefits feature content (testing)
test.feature_corpus <- jp_test$requirements %>% VectorSource %>% VCorpus %>% clean_corpus
fraud_col <- jp_train$fraudulent
rm(jp_test)
rm(jp_train)
gc()

#create a document term matrix for the training partition for the requirements attribute
train_dtm <- train.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

#create a document term matrix for the testing partition for the requirements attribute
test_dtm <- test.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

train_df <- intersect(colnames(train_dtm), colnames(test_dtm)) %>% train_dtm[,.] %>% data.frame
train_df$corpus <- fraud_col
test_df <- intersect(colnames(test_dtm), colnames(train_dtm)) %>% test_dtm[,.] %>% data.frame
rm(train_dtm)
rm(test_dtm)
gc()
test_df$corpus <- c("temp")

#training ksvm model on training data
trained_ksvm <- ksvm(corpus~., data=train_df, kernel="rbfdot")
gc()
#predicting training data from trained model
predictions <- predict(trained_ksvm, train_df)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1)
confusion_matrix <- confusionMatrix(factor(predictions), factor(train_df$corpus), positive = '1')
print(confusion_matrix)

#using trained model to make a prediction from test data
predictions <- predict(trained_ksvm, test_df)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
pred_df <- as.data.frame(predictions);
rownames(pred_df) <- rownames(predictions)
jp_test <- read.csv('jp_test.csv')
confusion_matrix <- confusionMatrix(factor(predictions), factor(jp_test$fraudulent), positive = '1')
print(confusion_matrix)