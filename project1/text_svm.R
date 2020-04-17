
#written by Francisco Baca

rm(list = ls())

###UNTESTED###
###INCOMPLETE###
library(magrittr) #for using pipes (more concise code, but slightly less explicit)

#text mining libraries
library(tm)
library(dplyr)
library(caret)  #for pre-processing, and model tuning
library(e1071) #for svm 
library(kernlab) #for svm
library(splitstackshape)

#removes numbers, whitespace, punctuation, stopwords,
# amongst other content that commonly interferes with
#text mining techniques 
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
#and validated partitions
jp_train <- read.csv('jp_train.csv')
jp_test <- read.csv('jp_test.csv')

#which columns contain text data elligible for text mining?
head(jp_train)

#columns eligible for text mining are benefits, description, requirements and company_profiles


#create and clean a dataframe of corpora for the above listed feature contents (training)
jp_train$benefits_corpus <- jp_train$benefits %<>% VectorSource %>% VCorpus %>% clean_corpus
jp_train$description_corpus <- jp_train$description %>% VectorSource %>% VCorpus %>% clean_corpus
jp_train$requirements_corpus <- jp_train$requirements %>% VectorSource %>% VCorpus %>% clean_corpus
jp_train$company_profile_corpus <- jp_train$company_profile %>% VectorSource %>% VCorpus %>% clean_corpus

#create and clean a dataframe of corpora for the above listed feature contents (testing)
jp_test$company_profile_corpus <- jp_test$company_profile %>% VectorSource %>% VCorpus %>% clean_corpus
jp_test$requirements_corpus <- jp_test$requirements %>% VectorSource %>% VCorpus %>% clean_corpus
jp_test$description_corpus <- jp_test$description %>% VectorSource %>% VCorpus %>% clean_corpus
jp_test$benefits_corpus <- jp_test$benefits %>% VectorSource %>% VCorpus %>% clean_corpus

#create document term matrices for the training partition
#might not work due to high memory usage, may remove later
#train_b_dtm <- jp_train$benefits_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
#train_d_dtm <- jp_train$description_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
#train_r_dtm <- jp_train$requirements_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
#train_cp_dtm <- jp_train$company_profile_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

#create a document term matrix for the testing partition
#might not work due to high memory usage, may remove later
#test_cp_dtm <- jp_test$company_profile_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
#test_r_dtm <- jp_test$requirements_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
#test_d_dtm <- jp_test$description_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()
#test_b_dtm <- jp_test$benefits_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

#create a document term matrix for the training partition for the company_profile attribute
train_dtm <- jp_train$company_profile_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()

#create a document term matrix for the testing partition for the benefits attribute
test_dtm <- jp_test$company_profile_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% as.matrix()


#document term matrices can likely only be set up for each text-based attribute one at a time,
#each one uses a lot of memory at one time and must be deleted once it is no longer being
#tested.