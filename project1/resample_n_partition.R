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
#END HELPER FUNCTIONS


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
jp <- 
  get_resample(jp_train, 'fraudulent', 1, .4) %>% 
  rbind(jp, .)

#writing training and testing partitions to file

write.csv(jp_train, 'jp_train.csv', row.names=F)
write.csv(jp_test, 'jp_test.csv', row.names=F)
