jp <- read.csv("./other_data/jp_prepared.csv")
install.packages("tibble")
library(tm) #to prepare document term matrices for text mining
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(magrittr) #for using pipes (more concise code, but slightly less explicit))
library(ggplot2) #for plotting histograms of tf-idf values across DTM terms
library(stringi) #for regex pattern matching and replacement
library(data.table) #for memory and speed optimization

#BEGIN HELPER FUNCTIONS
#returns new sample from given df, where p is 
#the desired percentage of rare records
#not necessary until setup phase
get_resample <- function(df, resampled_feature, rare_val, p){
	num_records <- dim(df)[1]
	rares_idx = which(df[resampled_feature] == rare_val)
	num_rares <- length(rares_idx)
	x <- ((p*num_records) - num_rares)/(1-p)
	resample_idx <- sample(x = rares_idx, size = x, replace=TRUE)
	return(df[resample_idx,])
}

clean_corpus <- function(corpus){
  corpus %<>% tm_map(., removeNumbers)
  corpus %<>% tm_map(., stripWhitespace)
  corpus %<>% tm_map(., removePunctuation)
  corpus %<>% tm_map(., content_transformer(tolower))
  corpus %<>% tm_map(., removeWords, stopwords("en"))
  corpus %<>% tm_map(., content_transformer(stemDocument), language = "english")
  
  return(corpus)
}


#returns tf-idf value for column x
#may not be necessary
get_tf_idf <- function(x){
  (length(x)/length(x[x>0])) %>%
  log2 %>%
  return
}

# Prints Z score and P value, returns Z score. Assumes int fields are 0: false and 1: true.
z_test <- function(df_train, df_test, field) {
  
  # Compute proportions
  prop_train <- sum(df_train[field] == 1) / dim(df_train)[1]
  prop_test <- sum(df_test[field] == 1) / dim(df_test)[1]
  prop_pooled <- ((sum(df_train[field] == 1) + sum(df_test[field] == 1)) / 
                     (dim(df_train)[1] + dim(df_test)[1]))
  
  # Compute Z score
  z <- ( (prop_train - prop_test) / 
           sqrt( prop_pooled * (1 - prop_pooled) * 
                  ((1/dim(df_test)[1]) + (1/dim(df_train)[1])) ) )
  
  cat("\nZ Score:", z, "\nOne-Tailed P Value:", pnorm(-abs(z)), "\nTwo-Tailed P value: ", (2 * pnorm(-abs(z))))
  return(z)
}

#END HELPER FUNCTIONS

#README
#So the reason HOP was not working is becuse you called it
#before the partitions were actually created in this script

#Also, I'm about to make you hate R, but it literally has something
#already built in for this... gonna use chi-square, look in lines
#following the assignment of jp_train and jp_test
# hom_of_prop(jp, jp_train, jp_test, 'employment_type')

#BEGIN DATA PREPARATION
unique(jp$fraudulent)
#output shows that the 'fraudulent' feature categories contain only {0, 1}
sum(jp$fraudulent)/dim(jp)[1] #gives % of fraudulent==TRUE in the dataset
#alternatively...
table(jp$fraudulent) #gives contingency table for fraudulent feature
#<TODO> consider adding proportions to the contingency table


#since only 4% of the records are fraudulent,
#the dataset needs rebalancing

#sorting by index field
#might not be necessary with new placeholder arg on line 42
#jp <- jp[order(jp$index),]

sum(jp$fraudulent)/dim(jp)[1]
#alternatively...
table(jp$fraudulent)
#post rebalancing, the proportions show a 50:50 
#fake-to-real ratio for the 'fraudulent' feature
#END DATA RESAMPLING AND REBALANCING


set.seed(7)
train_indices <- runif(dim(jp)[1]) < .75

jp_train <- jp[train_indices,]
jp_test <- jp[!train_indices,]

# T TESTS

# No numeric fields - we are using the binned salary fields in our models.


# Z TESTS

# Tests to perform are on telecom., has_logo, has_questions, fraudulent
z_test_vector <- c("telecommuting", "has_company_logo", "has_questions",
                   "fraudulent")

for (i in z_test_vector) {
  cat("\n\n--------------------\nTESTING", i, ":")
  z_test(jp_train, jp_test, i)
}

# All Two-Tailed P values are large, so this partition is valid.

#CHI-SQUARE TEST FOR HOMOGENEITY OF PROPORTIONS

#Determine which attributes require test
#print the number of unique values for each column
sapply(jp, function(x){
  x %>% unique %>% length %>%
  return
})


#features that qualify for the test are employment_type,
#industry, func, min_salary_binned, max_salary_binned, required_exp_num
# and sal_range_binned
would_chi_square <- c("employment_type", "industry", "func", 
                      "max_salary_binned", "min_salary_binned", 
                      "required_exp_num", "sal_range_binned")

#We define our null hypotheses to be that:
#the proportions for the values corresponding to the 
#above attributes differ significantly between the training(observed)
#and original (expected) dataset
#We also define our alternative hypothesis to be the logical
#negation of the null hypothesis
#The loop below prints column names that pass
#the test according to the p-value resulting 
#from chi-square test for HOP
for(i in would_chi_square){
  test_result <- table(jp_train[,i]) %>% 
  rbind(., table(jp[,i])) %>%
  chisq.test

  if(test_result$p.value >= 0.05){
    print(paste(c("Attribute", i, "passed the test with p-value", test_result$p.value), collapse =" "))
    print("The p-value is greater than (or equal to) 0.05, so we reject our null hypothesis for this attribute.")
  }else{
    print(paste(c("Attribute", i, "failed the test with p-value", test_result$p.value), collapse =" "))
    print("The p-value is less than 0.05, so we accept our null hypothesis for this attribute.")
  }
  #to separate the lines
  cat("\n")
}

#As you can see, all suspect attributes pass the test


#BEGIN DATA RESAMPLING AND REBALANCING
#rebalance at 50/50 on 'fraudulent' attribute
#not necessary until setup phase
jp_train <- 
  get_resample(jp_train, 'fraudulent', 1, .4) %>% 
  rbind(jp_train, .)


# col_tf_idfs <- sapply(dtm, get_tf_idf)
# col_tf_idfs %>% unique %>% sort(decreasing=F)

# #plotting continuous distribution of tf-idf values across dtm terms (columns)
# ggplot(as.data.frame(col_tf_idfs)) + geom_histogram(aes(x=col_tf_idfs))

#benefits
#SAVING DTM TRAINING DATA FOR benefits ATTRIBUTE
jp.feature_corpus <- jp_train$benefits %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_train <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_train$fraudulent <- jp_train$fraudulent %>% factor
dtm_train %<>% as.data.table()

#WRITING DTM TRAINING DATA FOR benefits TO FILE
fwrite(dtm_train, "./text_data/benefits_train_DTM.csv", row.names=F)


#SAVING DTM TESTING DATA FOR benefits ATTRIBUTE
jp.feature_corpus <- jp_test$benefits %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_test <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_test$fraudulent <- jp_test$fraudulent %>% factor
dtm_test %<>% as.data.table()


#WRITING DTM TESTING DATA FOR benefits TO FILE
fwrite(dtm_test, "./text_data/benefits_test_DTM.csv", row.names=F)


#description
#SAVING DTM TRAINING DATA FOR description ATTRIBUTE
jp.feature_corpus <- jp_train$description %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_train <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_train$fraudulent <- jp_train$fraudulent %>% factor
dtm_train %<>% as.data.table()

#WRITING DTM TRAINING DATA FOR description TO FILE
fwrite(dtm_train, "./text_data/description_train_DTM.csv", row.names=F)

#SAVING DTM TESTING DATA FOR description ATTRIBUTE
jp.feature_corpus <- jp_test$description %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_test <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_test$fraudulent <- jp_test$fraudulent %>% factor
dtm_test %<>% as.data.table()

#WRITING DTM TESTING DATA FOR description TO FILE
fwrite(dtm_test, "./text_data/description_test_DTM.csv", row.names=F)

# Finding impactful values in description attribute

dtm_impact_test <- data.table(matrix(nrow=dim(jp_test)[1]))
dtm_impact_train <- data.table(matrix(nrow=dim(jp_train)[1]))

for (i in colnames(dtm_test)) {
  # Looking for words that have a significant impact on the target variable
  if (i == "fraudulent") { break }
  tab <- table(dtm_test[[i]], dtm_test$fraudulent)
  freq <- tab[2,2] / (tab[2,1] + tab[2,2])
  tot <- tab[2,2] + tab[2,1]
  if (freq > 0.40 && tot > 10) {
    dtm_impact_train[[i]] <- dtm_train[[i]]
    dtm_impact_test[[i]] <- dtm_test[[i]]
  }
}

dtm_impact_test$V1 <- NULL
dtm_impact_train$V1 <- NULL
dtm_impact_test$fraudulent <- dtm_test$fraudulent
dtm_impact_train$fraudulent <- dtm_train$fraudulent

# Write impactful data to file
fwrite(dtm_impact_test, "./text_data/impact_description_test_DTM.csv", row.names=F)
fwrite(dtm_impact_train, "./text_data/impact_description_train_DTM.csv", row.names=F)

#requirements
#SAVING DTM TRAINING DATA FOR requirements ATTRIBUTE
jp.feature_corpus <- jp_train$requirements %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_train <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_train$fraudulent <- jp_train$fraudulent %>% factor
dtm_train %<>% as.data.table()

#WRITING DTM TRAINING DATA FOR requirements TO FILE
fwrite(dtm_train, "./text_data/requirements_train_DTM.csv", row.names=F)

#SAVING DTM TESTING DATA FOR requirements ATTRIBUTE
jp.feature_corpus <- jp_test$requirements %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_test <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_test$fraudulent <- jp_test$fraudulent %>% factor
dtm_test %<>% as.data.table()

#WRITING DTM TESTING DATA FOR requirements TO FILE
fwrite(dtm_test, "./text_data/requirements_test_DTM.csv", row.names=F)



#company_profiles
#SAVING DTM TRAINING DATA FOR company_profile ATTRIBUTE
jp.feature_corpus <- jp_train$company_profile %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_train <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_train$fraudulent <- jp_train$fraudulent %>% factor
dtm_train %<>% as.data.table()

#WRITING DTM TRAINING DATA FOR company_profile TO FILE
fwrite(dtm_train, "./text_data/company_profiles_train_DTM.csv", row.names=F)

#SAVING DTM TESTING DATA FOR company_profile ATTRIBUTE
jp.feature_corpus <- jp_test$company_profile %>%
                      as.character %>% 
                      stri_replace_all_regex(str=., pattern = "[^A-Za-z\\s]+", replacement = "")

jp.feature_corpus %<>% VectorSource 
jp.feature_corpus %<>% VCorpus 
jp.feature_corpus %<>% clean_corpus
gc()

dtm_test <- jp.feature_corpus %>% DocumentTermMatrix(., control=list(wordLengths=c(1, Inf))) %>% 
                         as.matrix %>% as.data.frame

dtm_test$fraudulent <- jp_test$fraudulent %>% factor
dtm_test %<>% as.data.table()

#WRITING DTM TESTING DATA FOR company_profile TO FILE
fwrite(dtm_test, "./text_data/company_profiles_test_DTM.csv", row.names=F)



#CROSS VALIDATION WILL BE PERFORMED WITHIN text_svm.R and Random Forests

#WRITING TRAINING AND TESTING PARTITIONS TO FILE

write.csv(jp_train, './other_data/jp_train.csv', row.names=F)
write.csv(jp_test, './other_data/jp_test.csv', row.names=F)
