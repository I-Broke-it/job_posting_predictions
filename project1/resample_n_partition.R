jp <- read.csv("jp_prepared.csv")

library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(magrittr) #for using pipes (more concise code, but slightly less explicit)

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
  
  cat("Z Score:", z, "\nOne-Tailed P Value:", pnorm(-abs(z)), "\nTwo-Tailed P value: ", (2 * pnorm(-abs(z))))
  return(z)
}

hom_of_prop <- function(df, df_train, df_test, field) {
  
  # Initialize values
  expected_train <- c(); expected_test <- c(); observed_train <- c(); observed_test <- c();
  n <- dim(df)[1]; n_train <- dim(df_train)[1]; n_test <- dim(df_test)[1];
  
  for (i in unique(df[field])) {
    
    # Compute Expected Values
    prop_all <- sum(df$field == i) / n
    append(expected_train, prop_all * n_train)
    append(expected_test, prop_all * n_test)
    
    # Compute Observed Values
    append(observed_train, sum(df_train$field == i))
    append(observed_train, sum(df_test$field == i))
  }
  
  observed <- rbind(observed_train, observed_test)
  expected <- rbind(expected_train, expected_test)
  
  chi.sq <- sum((observed - expected)^2 / expected)
  p <- 1 - pchisq(chi.sq, length(unique(df[field])) - 1)
  cat("Chi Squared: ", chi.sq, "\nP-Value:")
}

#END HELPER FUNCTIONS

hom_of_prop(jp, jp_train, jp_test, 'employment_type')

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



#<TODO> Partition validation

#BEGIN DATA RESAMPLING AND REBALANCING
#rebalance at 50/50 on 'fraudulent' attribute
#not necessary until setup phase
jp_train <- 
  get_resample(jp_train, 'fraudulent', 1, .4) %>% 
  rbind(jp_train, .)

#writing training and testing partitions to file

write.csv(jp_train, 'jp_train.csv', row.names=F)
write.csv(jp_test, 'jp_test.csv', row.names=F)
