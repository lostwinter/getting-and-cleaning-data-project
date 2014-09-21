# #PROJECT FOR GETTING AND CLEANING DATA
# 0. Dataset source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names. 
# 5. From the data set in step 4, create a second, independent tidy data set with the average of 
#    each variable for each activity and each subject.
#________________________________________
# Dependency packages
require(plyr)

######################
# 0. Download zip and put UCI HAR Dataset folder into the wd
######################

######################
# 0.1 Make directory and add files
######################
uciDir <- "UCI/HAR/Dataset"
feature_file <- paste(uciDir, "/features.txt", sep = "")
activity_labels_file <- paste(uciDir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uciDir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uciDir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uciDir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uciDir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uciDir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uciDir, "/test/subject_test.txt", sep = "")

#####################
# 0.2 Load data
#####################
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

######################
# 1. Merge the training and the test sets to create one data set.
######################

# a) Bind data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# b) Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

#####################
# 2. Extract the measurements on the mean and standard deviation for each measurement.
#####################
sensor_data_mean_sd <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

#####################
# 3. Use descriptive activity names to name the activities in the data set.
#####################
sensor_data_mean_sd <- join(sensor_data_mean_sd, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_sd <- sensor_data_mean_sd[,-1]

#####################
# 4. Label the data set with descriptive names.
#####################
# a) Make valid names.
names(sensor_data_mean_sd) <- gsub('\\(|\\)',"",names(sensor_data_mean_sd), perl = TRUE)
names(sensor_data_mean_sd) <- make.names(names(sensor_data_mean_sd))
# b) Better names.
names(sensor_data_mean_sd) <- gsub('Acc',"Acceleration",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('Mag',"Magnitude",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('\\.mean',"Mean",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('\\.std',"StandardDeviation",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_sd))
names(sensor_data_mean_sd) <- gsub('Freq$',"Frequency",names(sensor_data_mean_sd))

#####################
# 5. Make a new tidy data set with the average of each variable 
#for each activity and each subject.
#####################
sensor_avg_by_act_sub = ddply(sensor_data_mean_sd, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt")

# End
