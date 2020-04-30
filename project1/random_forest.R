
#written by Francisco Baca

#TESTING ALTERNATIVE MODEL C5 RANDOM FORESTS ON THE DATA
rm(list = ls())
gc()

###INCOMPLETE###
library(magrittr) #for using pipes (more concise code, but slightly less explicit)
#text mining libraries
library(tm)
library(dplyr)
library(caret)  #for pre-processing, and model tuning
library(C50)
library(kernlab)
library(data.table) #for memory efficiency
library(doParallel)
library(stringi)

#not sure if necessary
library(psych) #for PCA if necessary

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

jp_train <- fread('./other_data/jp_train.csv')


# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)

#columns eligible for text mining are benefits, description, requirements and company_profiles


#create and clean a dataframe of corpora for the benefits feature content 
jp.feature_corpus <- jp$fraudulent %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm$fraudulent <- jp$fraudulent %>% factor
dtm %<>% as.data.table()
gc()

#non-parallel
C5 <- C5.0(fraudulent~., data=dtm, control = C5.0Control(minCases=75), subset=T)
predictions <- predict(C5, jp_train$fraudulent)
#<TODO> CREATE, OPTIMIZE AND PREPARE DOCUMENT TERM MATRICES FOR benefits, description,
# requirements AND company_profiles ATTRIBUTES FOR THE MODELS



# pca01 <- kpca(corpus~.,data=dtm, kernel='rbfdot', kpar=list(sigma=0.2), features=5)

# temp <- sapply(dtm, function(x){

#   unique(x) %>%
#   length() %>% 
#   return()
# })

#consider kernelizing this for dim reduction 
# pca01 <- principal(r = dtm, rotate="varimax", nfactors=5)


#parallel stuff begins here
# train_control <- trainControl(method="cv", number=2, verboseIter=T, allowParallel=T)
# model_grid <- expand.grid(.trials=5,
#                            .winnow=c(FALSE),
#                            .model=c("tree"))

# C5_CV <- train(fraudulent~., data=dtm, method="C5.0", tuneGrid=model_grid, subset=T, trControl=train_control)
# stopCluster(cl)