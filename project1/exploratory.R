# Written by Alexander Schulz

library(tm) #text mining library (removing stopwords, etc), delete if unnecessary
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(ggplot2) #plotting library. I like Rstudio, sue me
library(magrittr)

jp <- read.csv("jp_prepared.csv")

##### EXPLORATORY PHASE #####

#assigning new types
sapply(jp, class)
#converting from default R-assigned types
jp$job_id %<>% as.character %>% as.integer
jp$company_profile %<>% as.character
jp$requirements %<>% as.character
jp$description %<>% as.character
jp$department %<>% as.character
jp$benefits %<>% as.character
jp$location %<>% as.character
jp$title %<>% as.character
jp$fraudulent %<>% factor
jp$index %<>% as.integer

table(jp$fraudulent) # 17008 real, 865 fake. We will need to rebalance this in phase 3.


###############################################
### Exploring relationships based on Salary ###
###############################################

# I think the salary listed on a job interview would have a big impact on whether or not
# a job offering is fake. For fake jobs, you want to make it as enticing as possible, and
# raising the salary is an easy way to do so.

ggplot(jp, aes(max_salary_binned)) + geom_bar(aes(fill = fraudulent))
# We can see that there are over 15000 NA's, the vast majority of records.

ggplot(jp, aes(max_salary_binned)) + geom_bar(aes(fill = fraudulent), position="fill")
# Max salary does appear to have an impact on whether the job is fraudulent.

ggplot(jp, aes(min_salary_binned)) + geom_bar(aes(fill = fraudulent), position="fill")
# Interestingly, fraudulent records appear to be less frequent records where the min salary is higher.

### Exploring relationships based on job details and other general fields ###
ggplot(jp, aes(industry)) + geom_bar()
# Far too messy to look at.

table(jp$industry, jp$fraudulent)
# We can clearly see that some fields have a higher prorportion than others.
# For example, Internet has no fake jobs out of over 1000, while Oil & Energy companies
# are littered with 40% fake listings out of 287.

table(jp$telecommuting, jp$fraudulent)
# 4% without telecommuting are fake, while 9% with telecommuting are.

ggplot(jp, aes(employment_type)) + geom_bar(aes(fill = fraudulent), position="fill")
# Part time seems to have the most fraudulent records, while temporary has the least.
# This goes against my hypothesis stated earlier, where people making fake listings
# want to make it enticing.

ggplot(jp, aes(required_experience)) + geom_bar(aes(fill = fraudulent), position="fill")
# While executive has the highest proportion, it also has the least records. Entry level
# seems to be the most common for fraudulent records, but it seems statistically equal
# across the board.

table(jp$required_education, jp$fraudulent)
# Jobs listed as "Some High School Coursework" are very likely to be fake. Other than that,
# the proportions seem to be equal.

table(jp$fun, jp$fraudulent)
# Administrative is nearly 1/5, but other than that it's even across the board.

table(jp$has_questions, jp$fraudulent)
# Jobs with questions are roughly four as likely to be real than those without. 
# 7% vs 2% respectively, though this is still a fairly small difference.

table(jp$has_company_logo, jp$fraudulent)
# 18.9% of the ones without a company logo are fake, compared to only 2% otherwise.
# This is a vast difference that I wasn't expecting.

table(jp$location, jp$fraudulent)
# Too many unique values. We should probably change this to Country.

table(is.na(jp$requirements), jp$fraudulent)
# There's no discernable difference here, so requirements doesn't seem to have an impact

table(is.na(jp$company_profile), jp$fraudulent)
# Fradulent records tend to not have a company profile.

table(is.na(jp$benefits), jp$fraudulent)
# Whether or not a record has benefits does not affect whether or not a record is fake.
# This makes sense, as it's typically the content of the benefits that would indicate this.


############################################
### Exploring multivariate relationships ###
############################################

#the graph shown below shows a multivariate relationship between min_salary, max_salary and the target variable fraudulent
ggplot(jp[!is.na(jp$max_salary) & !is.na(jp$min_salary),], aes(max_salary, min_salary, col=factor(fraudulent))) + 
  geom_point() + geom_smooth(method = 'lm', se=F)
# For records listing the same max salary, fraudulent records appear to have a somewhat higher
# minimum salary than their real counterparts. I'm not sure if this is statistically 
# significant though. 

table(jp$has_company_logo, jp$has_questions | jp$telecommuting)
# For companies without a logo, roughly 2/3 also didn't have questions or telecommuting
# listed on their profile. The company logo is a fairly big indicator of whether a job
# listing is fake or not, and it appears that these listing were put together quickly.


###########################################################
### Adding new fields and exploring their relationships ###
###########################################################

jp$salary_range <- jp$max_salary - jp$min_salary
hist(jp$salary_range)
# Appears to be decreasing exponentially, no patterns. Binning with 0, 0.1, 10000, 30000, 50000, and 75000+.
sal_range_breaks <- c(0, 0.1, 10000, 30000, 50000, 75000, 200000)
sal_range_labels <- c("0", "0 - 10k", "10k - 30k", 
                      "30k - 50k", "50k - 75k", "75k+")

jp$sal_range_binned <- cut(x=jp$salary_range, breaks = sal_range_breaks, 
                            left=FALSE, labels = sal_range_labels)

ggplot(jp, aes(sal_range_binned)) + geom_bar(aes(fill=fraudulent))
# Overwhelmingly NA, but the majority of real records are in the 0 to 10k range.

ggplot(jp, aes(sal_range_binned)) + geom_bar(aes(fill=fraudulent), position = "fill")
# 0-10k and 50-75k seem to have the most records be fraudulent. Records with a 75k+ 
# difference seem to all be real.

# A well defined job has questions, a company logo, and telecommuting.
# Somehow in converting these fields to a factor, they were replaced with 2/1 instead of 1/0.
# Because of this, it the code is pretty messy.
jp$well_defined_job <- (jp$telecommuting & jp$has_company_logo & jp$has_company_logo)

# A sorta defined job has at least 1 of questions, a company logo, or telecommuting.
jp$sorta_defined_job <- (jp$telecommuting | jp$has_company_logo | jp$has_company_logo)

table(jp$well_defined_job, jp$fraudulent)
# Not too big of a difference here.

table(jp$sorta_defined_job, jp$fraudulent)
# This field is a very big indicator of if a job is fraudulent, as stated previously.

jp <- write.csv(jp,'jp_prepared.csv', row.names = F)

