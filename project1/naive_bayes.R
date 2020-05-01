# Written by Alexander Schulz

# memory wipe to prevent inconsistencies
rm(list = ls())
gc()

install.packages("tidyverse")
# libraries and installs
library(ggplot2)
library(e1071)
library(magrittr)

# Load train, test, and full sets. This might take a VERY long time,
# depending on how much RAM is in your system.
jp <- read.csv('./other_data/jp_prepared.csv')
jp_train_notext <- read.csv('./other_data/jp_train.csv')
jp_test_notext <- read.csv('./other_data/jp_test.csv')
text_class_train <- read.csv("./text_data/impact_description_train_DTM.csv")
text_class_test <- read.csv("./text_data/impact_description_test_DTM.csv")

jp_train <- cbind(jp_train_notext, text_class_train)
jp_test <- cbind(jp_test_notext, text_class_test)


##############################
##### ESTABLISH BASELINE #####
##############################

table(jp$fraudulent)
# Our model can get 95% accuracy by claiming 
# all jobs are non-fraudulent. In practice, this
# isn't really achievable.
# False negatives are more deadly than false positives,
# so we should have a model that has high recall.
# 80% is the minimum recall we want.


#########################
###### MODEL DATA #######
#########################

# This will do basic analysis on the models I build.
# Further evaluation on the best model is done below.

# Housekeeping
jp_train$fraudulent <- as.logical(jp_train$fraudulent)
jp_test$fraudulent <- as.logical(jp_test$fraudulent)


# USING ONLY SALARY

nb_sal_only <- naiveBayes(formula = fraudulent ~ min_salary_binned + max_salary_binned,
                          data = jp_train)

nb_sal_only

ypred_sal_only <- predict(object = nb_sal_only, newdata = jp_test)
table(jp_test$fraudulent, ypred_sal_only)
# Terrible model with terrible recall, it's like 5%. These fields have lots
# of NAs. Using this field seems promissing though.

# USING GOODNESS OF PROFILE

nb_prof <- naiveBayes(formula = fraudulent ~ telecommuting + has_company_logo
                      + has_questions, data = jp_train)
nb_prof

ypred_prof <- predict(object = nb_prof, newdata = jp_test)
table(jp_test$fraudulent, ypred_prof)
# 73% recall. Doesn't meet baseline.

# USING REQUIREMENTS AND TYPE

nb_reqs <- naiveBayes(formula = fraudulent ~ required_experience + required_education +
                        employment_type, data = jp_train)

nb_reqs

ypred_reqs <- predict(object = nb_reqs, newdata = jp_test)
table(jp_test$fraudulent, ypred_reqs)
# 30% recall again. We might need more fields.


# USING ALL FACTORS MINUS SALARY

nb_all <- naiveBayes(formula = fraudulent ~ required_experience + required_education +
                        employment_type + telecommuting + has_company_logo
                      + has_questions + industry + func, data = jp_train)
nb_all

ypred_all <- predict(object = nb_all, newdata = jp_test)
table(jp_test$fraudulent, ypred_all)
# 77% recall. Close, but stil not above our baseline.


# CLEAREST PROBABILITIES
# This uses the fields that had the clearest probabilities.
nb_clear <- naiveBayes(formula = fraudulent ~ industry + func + has_company_logo
                       + employment_type + required_education + required_experience,
                       data = jp_train)


nb_clear

ypred_clear <- predict(object = nb_clear, newdata = jp_test)
table(jp_test$fraudulent, ypred_clear)
# 79%. SO close, but doesn't round up.


# ALL WITHIN REASON

# Uses all independant fields with less than 1000 factors.
# We determined that several fields were related
# in phase II. Because Naive Bayes assumes that all predictors are independant,
# we can't use them together without hurting the model.

nb_super_all <- naiveBayes(formula = fraudulent ~ industry + func + employment_type 
                           + required_education + required_experience
                           + max_salary_binned + sorta_defined_job
                           + department, data = jp_train)

nb_super_all

ypred_super <- predict(object = nb_super_all, newdata = jp_test)
table(jp_test$fraudulent, ypred_super)
# 81%, just beats out the baseline.


# Text fields
nb_text <- naiveBayes(formula = fraudulent ~ industry + func + employment_type 
                      + required_education + required_experience
                      + max_salary_binned + sorta_defined_job
                      + department + decid + discoveri + fullservic + hazard
                      + inquir + mold + offshor + overviewak + petroleum
                      + valuesw + wifi + worker, data = jp_train)

nb_text
ypred_text <- predict(object = nb_text, newdata = jp_test)
table(jp_test$fraudulent, ypred_text)
# High accuracy, but at the sacrifice of recall. A very expensive model though,
# false negatives are abundant. If you're looking for a very accuract model
# though, this is the one. It just barely misses out on the all positive
# model.

############################
##### EVALUATION PHASE #####
############################



# Clean table setup
tabMem <- table(jp_test$fraudulent, ypred_super)
colnames(tabMem) <- c("Predicted False", "Predicted True")
rownames(tabMem) <- c("Actual False", "Actual True")
tabMem <- addmargins(A = tabMem, FUN = list(Total = sum), quiet = TRUE)

# Show table
tabMem

# Calculate statistics regarding data.
acc <- (tabMem[1,1] + tabMem[2,2])/tabMem[3,3]
# 80%. Not bad.

err <- 1 - acc
# 19%.

sensitivity <- tabMem[2,2]/tabMem[2,3]
# 81%. Fairly solid.

specificity <- tabMem[1,2]/tabMem[1,3]
# 19%, which is abysmal, but we sacrificed this for Sensitivity/Recall.

precision <- tabMem[2,2]/tabMem[3,2]
# 17%, meaning most positives are actually false.

recall <- sensitivity
# also 81%.

cat("Accuracy: ", acc)
cat("Error rate: ", err)
cat("Sensitivity/Recall: ", sensitivity)
cat("Specificity: ", specificity)
cat("Precision: ", precision)

# In general, this model meets our cost metrics. False Negatives are deadly,
# as they mean giving out PII to malicious individuals. Our goal here is to 
# identify as many of these as possible. Ideally, we would want to lower
# the number of false positives, as there are a ton. However, this result
# is passable.