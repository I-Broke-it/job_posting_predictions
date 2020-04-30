#Written by Francisco Baca

rm(list = ls())

library(tm) #text mining library (removing stopwords, etc), delete if unnecessary
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(ggplot2) #plotting library
library(magrittr) #for using pipes (more concise code, but slightly less explicit)
library(tableHTML) #for viewing dataframes in browser (I don't like rstudio, sue me)

jp <- read.csv('./other_data/fake_job_postings.csv')

#BEGIN MISSING AND MISLEADING DATA HANDLING

#assigning new types
#performed to avoid any further errors (yeah... right)
sapply(jp, class)
#converting from default R-assigned types
jp$job_id %<>% as.character %>% as.integer
jp$company_profile %<>% as.character
jp$requirements %<>% as.character
jp$description %<>% as.character
jp$department %<>% as.character
jp$has_company_logo %<>% factor
jp$benefits %<>% as.character
jp$location %<>% as.character
jp$telecommuting %<>% factor
jp$has_questions %<>% factor
jp$title %<>% as.character
jp$fraudulent %<>% factor
jp$index %<>% as.integer


#some troublesome entries in jp_rebal$salary_range contain non-numeric characters such
#as 'Nov', 'Dec', etc...
#first we identify acceptable entries via regular expression
#we create a list of jp_rebal indices for acceptable salary ranges
acceptable_salary_ranges <- 
	regexpr("[[:digit:]]+[-][[:digit:]]", jp$salary_range) > 0 

#removing NA's from acceptable salary index list
acceptable_salary_ranges <- acceptable_salary_ranges & 
	!is.na(jp$salary_range)

#now we check that we can subset and identify unacceptable salary ranges
#save for entries marked NA
jp[!acceptable_salary_ranges,]$salary_range %>%
	unique()

#clearly the resulting cells contain misleading data
#so we replace their contents with NA
jp[!acceptable_salary_ranges,]$salary_range  <- NA

#split salary_range on '-' to make min_salary and max_salary columns
min_max_sal_cols <- jp$salary_range %>% as.character() %>% strsplit(., '-')
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
#merging jp and min_max_sal_df column_wise
jp <- cbind(jp, min_max_sal_df)
#removing min_max_sal_cols
rm(min_max_sal_cols)
#removing min_max_sal_df
rm(min_max_sal_df)

#dropping salary_range attribute
jp <- jp[,(colnames(jp) != c('salary_range'))]

#changing problematic column name ('function.' -> 'func')
colnames(jp)[which(colnames(jp)=='function.')] <- 'func'

#replacing missing data '' with NA
jp <- apply(jp, MARGIN = 2, as.character) 

jp <- 
	apply(jp, MARGIN = 2, function(x){
		x <- x %>% 
			as.character() %>%
			ifelse(test=.=='', yes=NA,no=.)
	}) %>%
	as.data.frame()

#verify blank replacement was successful
blanks <- sapply(jp, function(x){
  table(x=='')
})
blanks %>% data.frame
rm(blanks)

#Exploring default R column types
col_types <- sapply(jp, class)

#the code below can be used to view
#25 random observations from the data
#in order minimize encouters with NA cells
jp[runif(25, 1, dim(jp)[1]),] %>% 
    tableHTML(rownames=FALSE)

#setting min_salary and max_salary as numeric
jp$min_salary <- as.numeric(levels(jp$min_salary))[jp$min_salary]
jp$max_salary <- as.numeric(levels(jp$max_salary))[jp$max_salary]

#several salaries are listed as 0, which does not make
#sense, so now we change these to NA and run z-score normalization
#on the salary attributes again
zero_indices <- (jp$min_salary == 0) & !is.na(jp$min_salary)
jp[zero_indices,]$min_salary <- NA
zero_indices <- (jp$max_salary == 0) & !is.na(jp$max_salary)
jp[zero_indices,]$max_salary <- NA

#plotting after zero salary removal
ggplot(jp[!is.na(jp$min_salary),], aes(min_salary)) + geom_histogram()
ggplot(jp[!is.na(jp$max_salary),], aes(max_salary)) + geom_histogram()
#clearly, there are outliers heavily skewing the distribution

#introduces more NAs but ensures no salary outliers remain by performing z-score
#and removing salary outliers until there are none left
repeat{
	jp$min_salary_z <- scale(x=jp$min_salary) %>% as.numeric
	jp$max_salary_z <- scale(x=jp$max_salary) %>% as.numeric

	#dictionary of outliers with keys 'max_salary' and 'min_salary'
	outlier_indices <- c()
	outlier_indices[['min_salary']] <- ((jp$min_salary_z > 3 |
										jp$min_salary_z < -3) &
										!is.na(jp$min_salary_z))

	outlier_indices[['max_salary']] <- ((jp$max_salary_z > 3 |
										jp$max_salary_z < -3) &
										!is.na(jp$max_salary_z))

	no_min_outliers_exist <- (dim(jp[outlier_indices[['min_salary']],])[1] == 0)
	no_max_outliers_exist <- (dim(jp[outlier_indices[['max_salary']],])[1] == 0)
	
	if(no_min_outliers_exist && no_max_outliers_exist){
		break
	}else if (no_max_outliers_exist) {
		jp[outlier_indices[['min_salary']],]$min_salary <- NA
	}else{
		jp[outlier_indices[['max_salary']],]$max_salary <- NA
	}

}

#plotting after salary outlier removal
ggplot(jp[!is.na(jp$min_salary),], aes(min_salary)) + geom_histogram()
ggplot(jp[!is.na(jp$max_salary),], aes(max_salary)) + geom_histogram()

#BEGIN BINNING 
min_sal_breaks <- c(0, 25000, 50000, 75000, 100000,500000,1e+06, 6e+06)
min_sal_labels <- c("<25e+03", "25e+03-50e+03", "50e+03-75e+03", "75e+03-1e+05", 
					"1e+05-5e+05", "5e+05-1e+06", "1e+06-6e+06")

jp$min_salary_binned <- cut(x=jp$min_salary, breaks = min_sal_breaks,
								right=FALSE, labels = min_sal_labels)

ggplot(jp[!is.na(jp$min_salary_binned),], aes(min_salary_binned)) + geom_bar(aes(fill=fraudulent))

max_sal_breaks <- c(0, 25000, 50000, 75000, 100000,500000,1e+06, 7e+06)
max_sal_labels <- c("<25e+03", "25e+03-50e+03", "50e+03-75e+03", "75e+03-1e+05",
					"1e+05-5e+05", "5e+05-1e+06", "1e+06-7e+06")

jp$max_salary_binned <- cut(x=jp$max_salary, breaks = max_sal_breaks, 
					right=FALSE, labels = max_sal_labels)

ggplot(jp[!is.na(jp$max_salary_binned),], aes(max_salary_binned)) + geom_bar(aes(fill=fraudulent))
#END OUTLIER HANDLING

#<TODO> Adjust binning for any changes regarding outliers
#END BINNING

jp$required_exp_num <- revalue(x= jp$required_experience, replace = c('Not Applicable' = NA, 'Internship' = 1, 'Executive' = 6,
																					'Entry level' = 2, 'Associate'=3,
																					'Mid-Senior level' = 4, 'Director' = 5))

jp$required_edu_num <- revalue(x=jp$required_education, replace = c('Some High School Coursework' = 1,
																				'High School or equivalent'=2, 'Vocational - HS Diploma'=3,
																				'Some College Coursework Completed'=4, 'Associate Degree'=5,
																				'Vocational'=6, 'Vocational - Degree'=7, 'Certification'=8,
																				"Bachelor's Degree"=9, 'Professional'=10, 
																				"Master's Degree"=11,'Doctorate'=12, 'Unspecified' = NA))

#assigning new types
#performed to avoid any further errors (yeah... right)
sapply(jp, class)
jp$job_id %<>% as.character %>% as.integer
#only those that need changing
jp$index %<>% as.integer
jp$title %<>% as.character
jp$location %<>% as.character
jp$department %<>% as.character
jp$company_profile %<>% as.character
jp$requirements %<>% as.character
jp$description %<>% as.character
jp$benefits %<>% as.character
jp$fraudulent %<>% factor

#add index field
jp$index <- c(1:dim(jp)[1])

#saving to file
write.csv(jp,'./other_data/jp_prepared.csv', row.names = F)
#END DATA PREPARATION
#END MISSING AND MISLEADING DATA HANDLING

