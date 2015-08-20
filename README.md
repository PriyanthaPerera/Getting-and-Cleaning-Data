# Getting-and-Cleaning-Data :Peer Assessment Course Project

##Course Assignment Getting and Cleaninig Data
##Author: Priyantha Perera
##Date: August 06 2015 - August 17 2015
##Code: run_analysis.R

* This code merges the given training and testing data sets. Appends subject codes 1-30 and activity codes 1-6
  to each observation
* Picks out the variable names which are either "mean()" or "std()" of the related signal. This extracts 66 
  features out of 561 features in the raw dataset.
* From the above selected data creates a tidy data set with the average of each variable for each activity and
  each subject.
* Labels the data set variables with descriptive feature variable names and descriptive activity names. The first
  two columns are "activity" name and "subject" number.The remaining feature variable names are of the form
 "Average of ......", where "......" stands for the related feature name.



