Implemented by Francisco Baca and Alexander Schulz

Through the use of a text classifier as well as C5 decision trees and naive Bayes to see if we can make accurate predictions
of which job postings in our dataset are fake and we will verify the validity of our results through model evaluation techniques. 
Given that this is a text-rich dataset, we are taking the opportunity to perform SVM text classification on the data to find the
most accurate text-rich predictor attribute. Due to issues in resolving the curse of dimensionality in a timely fashion, SVM is
yet to be tested on the dataset.

  #CLARIFICATION FOR THE GRADER
  The files developed for CS 4331 project 2 are resample_n_partition.R, text_svm.R, naive_bayes.R,
  text_C5.R and any csv files they may produce. All other files correspond to work done for project 1.

- data_preparation uses original dataset file "fake_job_postings.csv" in order to perform its function. 
- exploratory.R depends on and alters the cleaned and prepared dataset file "jp_prepared.csv" produced 
  by data_preparation.R.
- resample_n_partition.R uses jp_prepared.csv to produced a resampled jp_train.csv a non-resampled
  jp_test.csv and csv files for the testing and training partitions of the Document-Term-Matrices extracted 
  from each text-rich column in the data. 

- Each of the models (text_svm.R, naive_bayes.R and text_C5.R) work with all of the csv files produced by
  the above scripts

  
  ***Requirements for running this***
  Make sure to download all the libraries used in the scripts. Due to the high memory usage of
  these models, it is recommended to add 'R_MAX_VSIZE=100Gb' to the .Renviron file which should
  be created in the user's home directory if it does not exist already. To avoid the possibility
  of exceeding max vector memory for R, it is recommended that you run this in the cli for R,
  with R --max-ppsize 5000. It is recommended that you experiment with the value of R_MAX_VSIZE
  as well as --max-ppsize, as performance may vary depending on your system's specs.

  Kaggle Dataset: https://www.kaggle.com/shivamb/real-or-fake-fake-jobposting-prediction
