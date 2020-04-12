#Written by Francisco Baca

library(tm) #text mining library (removing stopwords, etc), delete if unnecessary
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
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
#<TODO> consider adding proportions to the contingency table


#since only 4% of the records are fraudulent,
#the dataset needs rebalancing

#BEGIN DATA RESAMPLING AND REBALANCING
#rebalance at 50/50 on 'fraudulent' attribute
jp_rebal <- 
  get_resample(jp, 'fraudulent', 1, .5) %>% 
  rbind(jp, .)

#sorting by index field
#might not be necessary with new placeholder arg on line 42
#jp_rebal <- jp_rebal[order(jp_rebal$index),]

sum(jp_rebal$fraudulent)/dim(jp_rebal)[1]
#alternatively...
table(jp_rebal$fraudulent)
#post rebalancing, the proportions show a 50:50 
#fake-to-real ratio for the 'fraudulent' feature
#END DATA RESAMPLING AND REBALANCING

#BEGIN MISSING AND MISLEADING DATA HANDLING
#some troublesome entries in jp_rebal$salary_range contain non-numeric characters such
#as 'Nov', 'Dec', etc...
#first we identify acceptable entries via regular expression
#we create a list of jp_rebal indices for acceptable salary ranges

acceptable_salary_ranges <- 
	regexpr("[[:digit:]]+[-][[:digit:]]", jp_rebal$salary_range) > 0 

#removing NA's from acceptable salary index list
acceptable_salary_ranges <- acceptable_salary_ranges & 
	!is.na(jp_rebal$salary_range)

#now we check that we can subset and identify unacceptable salary ranges
#save for entries marked NA
jp_rebal[!acceptable_salary_ranges,]$salary_range %>%
	unique()

#clearly the resulting cells contain misleading data
#so we replace their contents with NA
jp_rebal[!acceptable_salary_ranges,]$salary_range  <- NA

#split salary_range on '-' to make min_salary and max_salary columns
min_max_sal_cols <- jp_rebal$salary_range %>% as.character() %>% strsplit(., '-')  
#preserving corresponding NA's for each column
min_max_sal_cols[is.na(min_max_sal_cols)] <- list(c(NA, NA))

					#converting to vector
min_max_sal_df <- min_max_sal_cols %>% unlist() %>% 
	#converting to matrix
	matrix(., nrow = length(min_max_sal_cols), byrow = TRUE) %>%
	#converting to dataframe
	data.frame(., stringsAsFactors = FALSE)
#renaming columns
colnames(min_max_sal_df) <- c('min_salary', 'max_salary')
#merging jp_rebal and min_max_sal_df column_wise
jp_rebal <- cbind(jp_rebal, min_max_sal_df)
#removing min_max_sal_cols
rm(min_max_sal_cols)
#removing min_max_sal_df
rm(min_max_sal_df)

#dropping salary_range attribute
jp_rebal <- jp_rebal[,(colnames(jp_rebal) != c('salary_range'))]

#changing problematic column name ('function.' -> 'func')
colnames(jp_rebal)[which(colnames(jp_rebal)=='function.')] <- 'func'

#replacing missing data '' with NA
jp_rebal <- apply(jp_rebal, MARGIN = 2, as.character) 

jp_rebal <- 
	apply(jp_rebal, MARGIN = 2, function(x){
		x <- x %>% 
			as.character() %>%
			ifelse(test=.=='', yes=NA,no=.)
	}) %>%
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