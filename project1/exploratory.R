# Written by Alexander Schulz

library(tm) #text mining library (removing stopwords, etc), delete if unnecessary
library(plyr) #dataframe manipulation library
library(dplyr) #dataframe manipulation library
library(ggplot2) #plotting library. I like Rstudio, sue me

jp <- read.csv("jp_prepared.csv")

##### EXPLORATORY PHASE #####

table(jp$fraudulent) # 17008 real, 865 fake. We will need to rebalance this in phase 3.

### Exploring based on Salary ###
# I think the salary listed on a job interview would have a big impact on whether or not
# a job offering is fake. For fake jobs, you want to make it as enticing as possible, and
# raising the salary is an easy way to do so.

ggplot(jp, aes(max_salary_binned)) + geom_bar(aes(fill = fraudulent))
# We can see that there are over 15000 NA's, the vast majority of records.

ggplot(jp, aes(max_salary_binned)) + geom_bar(aes(fill = fraudulent), position="fill")
# Max salary does appear to have an impact on whether the job is fraudulent.

ggplot(jp, aes(min_salary_binned)) + geom_bar(aes(fill = fraudulent), position="fill")
# Interestingly, fraudulent records appear to be less frequent records where the min salary is higher.

### Exploring job details ###
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