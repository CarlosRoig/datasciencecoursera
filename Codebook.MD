CodeBook to clean and tidy the UCI HAR Dataset
=================================

## Exercise summary

The Coursera exercise demands that we create a file named `run.analysis.R` that cleans the UCI HAR dataset provided and writes a final tidy dataset file.

## How to use the script
To use and test the script you have to download the UCI HAR Dataset from here:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Then download the script `run.analysis.R` from this repository.

Then it's necessary to set your R working directory in the directory where you have unzipped the UCI HAR data.

To finish you only need to load the script executing: `source("run.analysis.R")`

## Variable transformations

The clean up works following the next steps:

### Step 1

The first step reads some two sets of data for further use. `features.txt` and `activity_labels.txt`.

### Steps 2 - 6

In this steps we merge the training data set and the test data set in a single data set.

As part of the clean up, we take only the variables that contain the mean or the standard deviation of the measurements.

### Step 7

In this step we exchange the activity number labels by their descriptive texts, completing the merging of the train and test data sets.

### Step 8

In this step we aggregate the data leaving a single row for each variable for each activity for each subject and we write it in a csv file called `UCI_HAR_tidydata.csv`

## Variables description

In the final data set we have the following variables:

- subjects: Variable to distinguish who made the experiment. It goes from 1 to 30
- activity: Variable to distinguish which activity the subject was experimenting. The activities are: 
	- 'laying',
	- 'sitting',
	- 'standing',
	- 'walking',
	- 'walking_downstairs'
	- 'walking_upstairs'
- measurements: the mean of 66 different measurements for each subject and activity. The measurements are the following:
	- tBodyAcc-mean()-X          
	- tBodyAcc-mean()-Y          
	- tBodyAcc-mean()-Z          
	- tBodyAcc-std()-X           
	- tBodyAcc-std()-Y           
	- tBodyAcc-std()-Z           
	- tGravityAcc-mean()-X       
	- tGravityAcc-mean()-Y       
	- tGravityAcc-mean()-Z       
	- tGravityAcc-std()-X        
	- tGravityAcc-std()-Y        
	- tGravityAcc-std()-Z        
	- tBodyAccJerk-mean()-X      
	- tBodyAccJerk-mean()-Y      
	- tBodyAccJerk-mean()-Z      
	- tBodyAccJerk-std()-X       
	- tBodyAccJerk-std()-Y       
	- tBodyAccJerk-std()-Z       
	- tBodyGyro-mean()-X         
	- tBodyGyro-mean()-Y         
	- tBodyGyro-mean()-Z         
	- tBodyGyro-std()-X          
	- tBodyGyro-std()-Y          
	- fBodyBodyGyroJerkMag-std() 
	- tBodyGyro-std()-Z          
	- tBodyGyroJerk-mean()-X     
	- tBodyGyroJerk-mean()-Y     
	- tBodyGyroJerk-mean()-Z     
	- tBodyGyroJerk-std()-X      
	- tBodyGyroJerk-std()-Y      
	- tBodyGyroJerk-std()-Z      
	- tBodyAccMag-mean()         
	- tBodyAccMag-std()          
	- tGravityAccMag-mean()      
	- tGravityAccMag-std()       
	- tBodyAccJerkMag-mean()     
	- tBodyAccJerkMag-std()      
	- tBodyGyroMag-mean()        
	- tBodyGyroMag-std()         
	- tBodyGyroJerkMag-mean()    
	- tBodyGyroJerkMag-std()     
	- fBodyAcc-mean()-X          
	- fBodyAcc-mean()-Y          
	- fBodyAcc-mean()-Z          
	- fBodyAcc-std()-X           
	- fBodyAcc-std()-Y           
	- fBodyAcc-std()-Z           
	- fBodyAccJerk-mean()-X      
	- fBodyAccJerk-mean()-Y      
	- fBodyAccJerk-mean()-Z      
	- fBodyAccJerk-std()-X       
	- fBodyAccJerk-std()-Y       
	- fBodyAccJerk-std()-Z       
	- fBodyGyro-mean()-X         
	- fBodyGyro-mean()-Y         
	- fBodyGyro-mean()-Z         
	- fBodyGyro-std()-X          
	- fBodyGyro-std()-Y          
	- fBodyGyro-std()-Z          
	- fBodyAccMag-mean()         
	- fBodyAccMag-std()          
	- fBodyBodyAccJerkMag-mean() 
	- fBodyBodyAccJerkMag-std()  
	- fBodyBodyGyroMag-mean()    
	- fBodyBodyGyroMag-std()     
	- fBodyBodyGyroJerkMag-mean()
