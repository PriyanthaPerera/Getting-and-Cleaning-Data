#Course Assignment Getting and Cleaninig Data
#Author: Priyantha Perera
#Date: August 06 2015 - August 17 2015

# This code merges the given training and testing data sets.

# Picks out the variable names which end in either "mean()" or "std()".

# Append descriptive activity names to name the activities in the data set.

# Labels the data set variables with descriptive variable names.

# From the above selected data creates a tidy data set with the average of each variable for each activity and each subject described by descriptive variable names.


library(Hmisc)
library(dplyr)
library(reshape2)


setwd("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset")

#Training data
#Describe X_train
X_train <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/train/X_train.txt", quote="\"")

y_train <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/train/y_train.txt", quote="\"")
#Describe subject_train
subject_train <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/train/subject_train.txt", quote="\"")

#Test data
#Describe X_test
X_test <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/test/X_test.txt", quote="\"")
#Describe X_test
y_test <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/test/y_test.txt", quote="\"")
#Describe subject_test
subject_test <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/test/subject_test.txt", quote="\"")


#Checking the dimensions for consistency
dim(X_train)
dim(X_test)
dim(y_train)
dim(y_test)
dim(subject_train)
dim(subject_test)


#Obtaining the unique cases
unique(subject_train)
unique(subject_test)
unique(y_train)
unique(y_test)

#Column binding the 6 activities and the 30 subject identifiers to the data. Last 2 columns to get extended data sets

traindata <- cbind(X_train,y_train,subject_train)
testdata <- cbind(X_test,y_test,subject_test)

#Row binding the extended train and test data sets, which now contain the activity and subject identifiers for each row vector.

alldata <-rbind(traindata,testdata)

#Check to see if proper number of  rows and columns in dataset

dim(alldata)

dim(traindata)[1]+dim(testdata)[1]
dim(traindata)[2]
dim(testdata)[2]


#Reading in the activity labels
activity_labels <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/activity_labels.txt", quote="\"")

#Reading in a text description file of feature information
features_info <- read.delim("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/features_info.txt", header=FALSE, stringsAsFactors=FALSE)

#Open the "features_info.txt" in a text reader to pick the appropriate signal names/variables for which mean() and std() were computed.

View(features_info)

#From the View of features_info  we picked out the rows with the signal names for which mean() and std() were computed. This is a visual scan and not automated! We will automate the selection further down the code:

(signal.names <- features_info[8:24,])

#Reading in all the 561 feature names
features <- read.table("C:/Users/Priyantha/Coursera/GettingAnd CleaningData/UCI HAR Dataset/features.txt", quote="\"", stringsAsFactors=FALSE)

dim(features)
head(features)

#The following function and code create an indicator function for the features whose names end in "-mean()" and "-std()" of these signals.

tmp <- NULL

columnlocator <- function(data,x) {

                    for (i in 1:dim(data)[1]){

                      ind <- ifelse(substring.location(data[[2]][i],x
                      )$first[1]>0,1,0)

                        if (is.null(tmp)) tmp<-ind else tmp<-c(tmp,ind)

                                    }
tmp
}


a <- columnlocator(features,"mean()")
b <- columnlocator(features,"std()")
c <- a+b
sum(c)
subsetnames <- features[c>0,]

#The following names the column variables in the "alldata" data.frame.

columnnames <- c(features[[2]],"activity1", "subject")
names(alldata) <- columnnames

####################################################################

#Following selects the required subset of data pertaining to the means - "means()" and standard deviatians - "std()" of the signals and appends the the activity and subject identifiers.Note we have ignored all other variables with "mean" or "Mean" in the variable name.  Essentially we have selected variables "***" which have their names ending in either "***mean()" or "***std()". Since the instructions were not clear on this point we have assumed thre above interpretation. We could have used the wider interpretation of including the other mean or Mean variables also.

requireddata <- alldata[,c(subsetnames[,2],"activity1", "subject")]
dim(requireddata)

##################
# creating vector "tmp2" of activity names to match the activity levels 1:6

factoractivityvec <- requireddata[,67]
tmp2 <- NULL
for (i in 1:10299) {4
          if(factoractivityvec[i]== "1")  tmp2[i] <- "WALKING"
          if(factoractivityvec[i]== "2")  tmp2[i] <- "WALKING_UPSTAIRS"
          if(factoractivityvec[i]== "3")  tmp2[i] <- "WALKING_DOWNSTAIRS"
          if(factoractivityvec[i]== "4")  tmp2[i] <- "SITTING"
          if(factoractivityvec[i]== "5")  tmp2[i] <- "STANDING"
          if(factoractivityvec[i]== "6")  tmp2[i] <- "LAYING"


}

# appending the activity names to the data.frame

requireddata <- mutate(requireddata, activityname = tmp2)

# Now we need to compute the averages of all activity mean()'s and std()'s in the "requireddata" data frame by activity name and subject and create a tidy data set (either long form or short form).

# First melt the data

datamelt <- melt(requireddata, id = c("activity1", "subject", "activityname"), measure.vars = subsetnames[,2])

#loop through each activity computing the mean of each column for each subject member and appending the appropriate activity name and row binding the variable averages in blocks of 30 subjects for all 6 activities, which is the required tidy data set in long form before appending descriptive column names for the averages

output<-NULL

for ( i in 1:6 ) {

   activitysubset <- subset(datamelt, requireddata$activity1==i)

                   A<- dcast(activitysubset, subject~variable, mean )


names(activitysubset)
activity <- c(unique(activitysubset$activityname))
activity
B <- cbind(activity,A)

head(B)
  ifelse( is.null(output), output <- B , output <- rbind(output,B))


}


newnames <- paste("Average_of_", subsetnames[,2], sep = "")

names(output) <- c("activity", "subject", newnames )

head(output)

# The dimensions of "output" should be 6 activities * 30 subjects = 180 rows and 66 signal means and standard deviation columns plus the activity name and subject id number = 68

dim(output)

# The data frame "output" is the required tidy data set in long form

head(output[output$activity =="WALKING",1:4],30)
#Checking the output
output[1:35,1:4]


write.table(output, "Getting and Cleaning Data Course Assignment Tidy Data Set.txt", row.names = FALSE)

