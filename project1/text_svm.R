
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

#removes whitespace, punctuation, stopwords,
# amongst other content that may negatively
#affect model performance
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


#columns eligible for text mining are benefits, description, requirements and company_profiles



jp.feature_corpus <- jp_train$benefits %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()
#create a document term matrix for the benefits attribute
dtm_train <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_train$fraudulent <- jp_train$fraudulent %>% factor
dtm_train %<>% as.data.table()

#training ksvm model on training data
trained_ksvm <- ksvm(fraudulent~., data=dtm_train, kernel="rbfdot", sigma=.2)
gc()
#predicting training data from trained model
predictions <- predict(trained_ksvm, dtm_train)
gc()
#mapping to target variable class values with .5 as the probability threshold
predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1)
confusion_matrix <- confusionMatrix(factor(predictions), factor(jp_train$fraudulent), positive = '1')
print(confusion_matrix)



jp.feature_corpus <- jp_test$benefits %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()
#create a document term matrix for the benefits attribute
dtm_test <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_test$fraudulent <- jp_test$fraudulent %>% factor
dtm_test %<>% as.data.table()
#testing trained model predictions on test data
predictions <- predict(trained_ksvm, dtm_test)
gc()

predictions <- ifelse(test=((1-predictions) >= .5), yes=0, no=1 )
predictions <- as.data.frame(predictions);
rownames(predictions) <- rownames(predictions)
confusion_matrix <- confusionMatrix(factor(predictions), factor(jp_test$fraudulent), positive = '1')
print(confusion_matrix)

gc()


