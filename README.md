#  RPROG

## Working directory for the Course Project on Getting and Cleaning Data.


This directory contains the R script, 'run_analysis.R' as required by the project. On running the R script, it does the following:


1.Downloads the dataset if it does not already exist in the working directory
2.Loads the activity and feature info
3.Loads both the training and test datasets, keeping only those columns which reflect a mean or standard deviation
4.Loads the activity and subject data for each dataset, and merges those columns with the dataset
5.Merges the two datasets
6.Converts the activity and subject columns into factors
7.Creates a tidy dataset that consists of the average (mean) value of each variable for each subject and activity pair.


The end result is shown in the file tidy.txt.

The result is uploaded as a link to the text file.


September 26, 2015