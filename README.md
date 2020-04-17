Implemented in collaboration with Alex Schulz

Through the use of a text classifier as well as other classification methods learned in class (decision trees, c5) to see if
we can make accurate predictions of which job postings in our dataset are fake and we will verify the validity of our results through model evaluation techniques
Given that this is a text-rich dataset, we are taking the opportunity to perform SVM text classification on the data to find the most accurate text-rich predictor
attribute.

- data_preparation uses original dataset file "fake_job_postings.csv" in order to perform its function. 
- exploratory.R depends on cleaned and prepared dataset file "jp_prepared.csv" produced by data_preparation.R.

Current Progress: all scripts except for txt_svm.R are fully functional. txt_svm.R requires further
development, testing and also depends on the rebalancing, partitioning the data as well as validation
of said partitions, which is part of Project II and yet to be implemented. 
Kaggle Dataset: https://www.kaggle.com/shivamb/real-or-fake-fake-jobposting-prediction
