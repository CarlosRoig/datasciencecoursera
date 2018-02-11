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
- measurements: the mean of 66 different measurements for each subject and activity
