#Written by Francisco Baca

library(tm) #text mining library (removing stopwords, etc), delete if unnecessary
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(magrittr) #for more readable code
library(tableHTML) #for viewing dataframes in browser (I don't like rstudio, sue me)

jp <- read.csv('fake_job_postings.csv')

#BEGIN HELPER FUNCTIONS
#returns new sample from given df, where p is 
#the desired percentage of rare records
get_resample <- function(df, resampled_feature, rare_val, p){
  num_records <- dim(df)[1]
  rares_idx = which(df[resampled_feature] == rare_val)
  num_rares <- length(rares_idx)
  x <- ((p*num_records) - num_rares)/(1-p)
  resample_idx <- sample(x = rares_idx, size = x, replace=TRUE)
  return(df[resample_idx,])
}
#END HELPER FUNCTIONS


#BEGIN EDA AND DATA PREPARATION
#add index field
jp$index <- c(1:dim(jp)[1])
unique(jp$fraudulent)
#output shows that the 'fraudulent' feature categories contain only {0, 1}
sum(jp$fraudulent)/dim(jp)[1] #gives % of fraudulent==TRUE in the dataset
#alternatively...
table(jp$fraudulent) #gives contingency table for fraudulent feature

#since only 4% of the records are fraudulent,
#the dataset needs rebalancing

#BEGIN DATA RESAMPLING AND REBALANCING
#rebalance at 50/50 on 'fraudulent' attribute
jp_rebal <- 
  get_resample(jp, 'fraudulent', 1, .5) %>% 
  rbind(jp)

#sorting by index field
jp_rebal <- jp_rebal[order(jp_rebal$index),]

sum(jp_rebal$fraudulent)/dim(jp_rebal)[1]
#alternatively...
table(jp_rebal$fraudulent)
#post rebalancing, the proportions show a 50:50 
#fake-to-real ratio for the 'fraudulent' feature
#END DATA RESAMPLING AND REBALANCING

#<TODO> <OBSCENITY> split on '-' and finish this pain-in-the-ass binning

#BEGIN MISSING AND MISLEADING DATA HANDLING
#changing problematic column name ('function.' -> 'func')
colnames(jp_rebal)[which(colnames(jp_rebal)=='function.')] <- 'func'

#replacing missing data '' with NA
jp_rebal <- apply(jp_rebal, MARGIN = 2, as.character) 

jp_rebal <- 
    apply(jp_rebal, MARGIN = 2, function(x){
        x <- as.character(x)
        x <- ifelse(test=x=='',
        yes=NA,no=x)}) %>%
    as.data.frame()

#verify blank replacement was successful
blanks <- sapply(jp_rebal, function(x){
  table(x=='')
})
blanks
rm(blanks)

#Exploring default R column types
col_types <- sapply(jp_rebal, class)

#<TODO> BEGIN BINNING

#first we identify numerical data
#we select 25 random observations from the data
#in order minimize encouters with NA cells
jp_rebal[runif(25, 1, dim(jp_rebal)[1]),] %>% 
    tableHTML(rownames=FALSE)

#a potential numerical column is 'salary_range'
#but entries/cell-contents are delimited by '-' 
#END BINNING

#counting number of unique values for each column
#used for determining which features should be 
#categorical (or factor) features
sapply(jp_rebal, function(x){length(unique(x))}) %>%
    sort(decreasing=FALSE)

#<TODO> from the output, the only likely candidate features to be 
#converted to categorical (or factor) features are 

#<TODO> assigning new types

#<TODO>BEGIN OUTLIER HANDLING
#END OUTLIER HANDLING
#END EDA AND DATA PREPARATION
#END MISSING AND MISLEADING DATA HANDLING