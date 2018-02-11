##### Step 1: First we load some common data (features and activity labels)  ########################

features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

###### Step 2:  Then we read the train and test x data to merge them together ######################

x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")
x_data <- rbind(x_train, x_test)
names(x_data) <- as.character(features[,2])

######## Step 3: We keep only the mean and std variables #############################
mean_sd.index <- grep("mean\\(\\)|std\\(\\)", features[,2])
x_data_filtered <- x_data[,mean_sd.index]

###### Step 4: Now we read the train and test y data and we merge them   ########################
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_data <- rbind(y_train, y_test)
names(y_data) <- "activity"

###### Step 5: Now we do the same with the train and test subjects   ####################
subjects_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subjects_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subjects_data <- rbind(subjects_train, subjects_test)
names(subjects_data) <- "subjects"


########## Step 6:  We merge all the data in a single data frame    ##################
merged_data <- cbind(subjects_data, x_data_filtered, y_data)

######## Step 7: Finally we substitute the activity lebels by their descriptive names  #########
complete_data <- merge(merged_data, activity_labels, by.x = "activity", by.y = "V1")
complete_data$activity <- complete_data$V2
complete_data$V2 <- NULL

####### Step 8: To end the excercise we create a tidy data set with the mean of each variable for each activity for each subject   #########
####### to do this we need the library reshape2

if(!require(reshape2)){install.packages("reshape2")}

meltdata <- melt(complete_data, id.vars = c("subjects", "activity"))
tidydata <- dcast(meltdata, subjects + activity ~ variable, fun.aggregate = mean, na.rm = TRUE)
write.table(tidydata, file = "./UCI HAR Dataset/UCI_HAR_tidydata.csv", row.names = FALSE)
