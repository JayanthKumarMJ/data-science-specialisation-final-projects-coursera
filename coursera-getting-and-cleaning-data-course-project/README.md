# Getting and Cleaning Data - Course Project

  The script run_analysis.R, takes the data set and does following.
  1. Reads the training and test data sets and merges them together into one.
  2. Reads the features.txt to filter only mean and standard deviation columns.
  3. Extracts only mean and standard deviation values for each observation.
  4. Reads activity labels and merges it mean and SD data set.
  5. Reads subjects and adds it to the data set for the final filtering and grouping.
  6. Calculates the mean of each varible for each subject for each activity type.
