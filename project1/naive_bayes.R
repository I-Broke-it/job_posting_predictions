# Written by Alexander Schulz

# memory wipe to prevent inconsistencies
rm(list = ls())
gc()

# libraries and installs
install.packages("e1071")

library(ggplot2)
library(e1071)
library(magrittr)

# Load train, test, and full sets
jp <- read.csv('jp_prepared.csv')
jp_train <- read.csv('jp_train.csv')
jp_test <- read.csv('jp_test.csv')



##############################
##### ESTABLISH BASELINE #####
##############################

table(jp$fraudulent)
# Our model can get 95% accuracy by claiming 
# all jobs are non-fraudulent.



#########################
###### MODEL DATA #######
#########################

# Housekeeping
jp_train$fraudulent <- as.logical(jp_train$fraudulent)
jp_test$fraudulent <- as.logical(jp_test$fraudulent)


# USING ONLY SALARY

nb_sal_only <- naiveBayes(formula = fraudulent ~ min_salary_binned + max_salary_binned,
                          data = jp_train)

nb_sal_only

ypred_sal_only <- predict(object = nb_sal_only, newdata = jp_test)
table(jp_test$fraudulent, ypred_sal_only)
# Underfitting? All values are negative. This is a terrible model.
# Most likely failed due to vast amount of NAs. No point in evaluating.


# USING GOODNESS OF PROFILE

nb_prof <- naiveBayes(formula = fraudulent ~ telecommuting + has_company_logo
                      + has_questions, data = jp_train)
nb_prof

ypred_prof <- predict(object = nb_prof, newdata = jp_test)
table(jp_test$fraudulent, ypred_prof)
# 91%. Fails the test.

# USING REQUIREMENTS AND TYPE

nb_reqs <- naiveBayes(formula = fraudulent ~ required_experience + required_education +
                        employment_type, data = jp_train)

nb_reqs

ypred_reqs <- predict(object = nb_reqs, newdata = jp_test)
table(jp_test$fraudulent, ypred_reqs)
# 95%, which is better but it's heavily biased to negatives.


# USING ALL FACTORS MINUS SALARY

nb_all <- naiveBayes(formula = fraudulent ~ required_experience + required_education +
                        employment_type + telecommuting + has_company_logo
                      + has_questions + industry + func, data = jp_train)
nb_all

ypred_all <- predict(object = nb_all, newdata = jp_test)
table(jp_test$fraudulent, ypred_all)
# 93% accuracy.


# CLEAREST PROBABILITIES
# This uses the fields that had the clearest probabilities.
nb_clear <- naiveBayes(formula = fraudulent ~ industry + func + has_company_logo
                       + employment_type + required_education + required_experience,
                       data = jp_train)


nb_clear

ypred_clear <- predict(object = nb_clear, newdata = jp_test)
table(jp_test$fraudulent, ypred_clear)
# 96%. Beats the baseline, and isn't biased towaredsnegatives.
