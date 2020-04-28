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


#CROSS VALIDATION WILL BE PERFORMED WITHIN text_svm.R and [insert other models developed here]

#writing training and testing partitions to file

write.csv(jp_train, 'jp_train.csv', row.names=F)
write.csv(jp_test, 'jp_test.csv', row.names=F)
