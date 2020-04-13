#Written by Francisco Baca

rm(list = ls())

library(tm) #text mining library (removing stopwords, etc), delete if unnecessary
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(ggplot2) #plotting library
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
	print(x)
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

#the code below can be used to view
#25 random observations from the data
#in order minimize encouters with NA cells
jp_rebal[runif(25, 1, dim(jp_rebal)[1]),] %>% 
    tableHTML(rownames=FALSE)

#setting min_salary and max_salary as numeric
jp_rebal$min_salary <- as.numeric(levels(jp_rebal$min_salary))[jp_rebal$min_salary]
jp_rebal$max_salary <- as.numeric(levels(jp_rebal$max_salary))[jp_rebal$max_salary]

ggplot(jp_rebal, aes(min_salary)) + geom_histogram()
ggplot(jp_rebal, aes(max_salary)) + geom_histogram()
#clearly, there are outliers heavily skewing the distribution
#<TODO>BEGIN OUTLIER HANDLING
#z-score is proven to be a better standardization method than min-max
#in the presence of outliers
jp_rebal$min_salary_z <- scale(x=jp_rebal$min_salary) %>% as.numeric
jp_rebal$max_salary_z <- scale(x=jp_rebal$max_salary) %>% as.numeric

#dictionary of outliers with keys 'max_salary' and 'min_salary'
outlier_indices <- c()
outlier_indices[['min_salary']] <- ((jp_rebal$min_salary_z > 3 |
									jp_rebal$min_salary_z < -3) &
									!is.na(jp_rebal$min_salary_z))

outlier_indices[['max_salary']] <- ((jp_rebal$max_salary_z > 3 |
									jp_rebal$max_salary_z < -3) &
									!is.na(jp_rebal$max_salary_z))
#identifying outliers
jp_rebal[outlier_indices[['min_salary']],] %>% tableHTML()
jp_rebal[outlier_indices[['max_salary']],] %>% tableHTML()
#we can see from the output tables that the outliers for
#the attritutes 'min_salary' and 'max_salary' correspond to
#the same records.

#considering that the outlier records for 'min_salary' and 'max_salary' are 
#flagged as non-fraudulent, as well as the fact that non-fraudulent made up
#96% of the records prior to rebalancing, we can remove these records considering
# as well that their removal does not affect any of our assumptions. Upon viewing 
#the min and max salaries for these records, it is clear that the records
#fraudulent attribute was misclassified when the data was entered.

#removing outlier records
jp_rebal <- jp_rebal[!outlier_indices[['min_salary']],]

#plotting after salary outlier removal
ggplot(jp_rebal, aes(min_salary)) + geom_histogram()
ggplot(jp_rebal, aes(max_salary)) + geom_histogram()

#still, several outliers remain, it is likely due
#to data entry errors in the salary ranges, resulting
#in unusually large range differences
#Now we create new column salary_range_diff containing
#respective range differences between min and max salary
jp_rebal$salary_range_diff <- (jp_rebal$max_salary - jp_rebal$min_salary)
#computing z-value for salary_range_diff column values
jp_rebal$salary_range_diff_z <- scale(jp_rebal$salary_range_diff) %>% as.numeric
#identifying outlier indices
outlier_indices[['salary_range_diff']] <- ((jp_rebal$salary_range_diff_z > 3 | 
											jp_rebal$salary_range_diff_z < -3) &
											!is.na(jp_rebal$salary_range_diff_z))

jp_rebal[outlier_indices[['salary_range_diff']],] %>% tableHTML()
jp_rebal <- jp_rebal[!outlier_indices[['salary_range_diff']],]

ggplot(jp_rebal, aes(min_salary)) + geom_histogram()
ggplot(jp_rebal, aes(max_salary)) + geom_histogram()


#BEGIN BINNING 
min_sal_breaks <- c(0, 25000, 50000, 75000, 100000,500000,1e+06, 6e+06)
min_sal_labels <- c("<25e+03", "25e+03-50e+03", "50e+03-75e+03", "75e+03-1e+05", 
					"1e+05-5e+05", "5e+05-1e+06", "1e+06-6e+06")

jp_rebal$min_salary_binned <- cut(x=jp_rebal$min_salary, breaks = min_sal_breaks,
								right=FALSE, labels = min_sal_labels)

ggplot(jp_rebal[!is.na(jp_rebal$min_salary_binned),], aes(min_salary_binned)) + geom_bar(aes(fill=fraudulent))

max_sal_breaks <- c(0, 25000, 50000, 75000, 100000,500000,1e+06, 6e+06)
max_sal_labels <- c("<25e+03", "25e+03-50e+03", "50e+03-75e+03", "75e+03-1e+05",
					"1e+05-5e+05", "5e+05-1e+06", "1e+06-7e+06")

jp_rebal$max_salary_binned<- cut(x=jp_rebal$max_salary, breaks = max_sal_breaks, 
					right=FALSE, labels = max_sal_labels)

ggplot(jp_rebal[!is.na(jp_rebal$max_salary_binned),], aes(max_salary_binned)) + geom_bar(aes(fill=fraudulent))

#<TODO> Adjust binning for any changes regarding outliers
#END BINNING

#<TODO> assigning new types
sapply(jp_rebal, class)
jp_rebal$job_id <- jp_rebal$job_id %>% as.character %>% as.numeric
#only those that need changing
jp_rebal$title <- jp_rebal$title %>% as.character
jp_rebal$department <- jp_rebal$department %>% as.character
jp_rebal$company_profile <- jp_rebal$company_profile %>% as.character
jp_rebal$description <- jp_rebal$description %>% as.character
jp_rebal$requirements <- jp_rebal$requirements %>% as.character
jp_rebal$benefits <- jp_rebal$benefits %>% as.character
#only those that need changing

#saving to file
write.csv(jp_rebal,'jp_prepared.csv', row.names = F)
#END OUTLIER HANDLING
#END EDA AND DATA PREPARATION
#END MISSING AND MISLEADING DATA HANDLING