###UNTESTED###
###INCOMPLETE###
rm(list = ls())
library(magrittr) #for using pipes (more concise code, but slightly less explicit)

#text mining libraries
library(tm)
library(dplyr)
library(caret)  #for pre-processing, and model tuning
library(e1071) #for svm 
library(kernlab) #for svm
library(splitstackshape)

#removes whitespace, punctuation, stopwords,
# amongst other content that interferes with
#text mining
clean_corpus <- function(corpus){
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

#create and clean a dataframe of corpora for the above listed feature contents (training)
jp_train_corpora$benefits <- jp_train$benefits %>% VectorSource %>% VCorpus %>% clean_corpus
jp_train_corpora$description <- jp_train$description %>% VectorSource %>% VCorpus %>% clean_corpus
jp_train_corpora$requirements <- jp_train$requirements %>% VectorSource %>% VCorpus %>% clean_corpus
jp_train_corpora$company_profile <- jp_train$company_profile %>% VectorSource %>% VCorpus %>% clean_corpus

#create and clean a dataframe of corpora for the above listed feature contents (training)
jp_test_corpora$company_profile <- jp_test$company_profile %>% VectorSource %>% VCorpus %>% clean_corpus
jp_test_corpora$requirements <- jp_test$requirements %>% VectorSource %>% VCorpus %>% clean_corpus
jp_test_corpora$description <- jp_test$description %>% VectorSource %>% VCorpus %>% clean_corpus
jp_test_corpora$benefits <- jp_test$benefits %>% VectorSource %>% VCorpus %>% clean_corpus

#create a document term matrix for the training partition
jp_train_dtm <- jp_train_corpora %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

#create a document term matrix for the testing partition
jp_test_dtm <- jp_test_corpora %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
