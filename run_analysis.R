#Assignment Track 3

###############################STEP 1##############################
#read lables
path_lbl <- "./UCI HAR Dataset/features.txt"
lbls <- read.table(path_lbl , as.is = T)
lbls <- lbls[,2]

#read - test data
path_test_d <- "./UCI HAR Dataset/test/X_test.txt"
test_set_d <- read.table(path_test_d, header = F)
colnames(test_set_d) <- lbls

path_test_l <- "./UCI HAR Dataset/test/y_test.txt"
test_activity <- read.table(path_test_l, header = F)

test_subject_d <- "./UCI HAR Dataset/test/subject_test.txt"
test_subject <- read.table(test_subject_d, header = F)

#bind test data
test_all <-cbind(test_subject,test_activity,test_set_d)

#read training data
path_train_d <- "./UCI HAR Dataset/train/X_train.txt"
train_t <- read.table(path_train_d, header = F)
colnames(train_t) <- lbls

path_train_l <- "./UCI HAR Dataset/train/y_train.txt"
train_activity <- read.table(path_train_l, header = F)

subject_train_p <- "./UCI HAR Dataset/train/subject_train.txt"
train_subject <- read.table(subject_train_p, header = F)

#bind train data
train_all <-cbind(train_subject,train_activity,train_t)

###############################STEP 2##############################
#Merges the training and the test sets to create one data set
all <-rbind(test_all,train_all)

#remove data frames from memory to save space
rm(test_set_d, test_activity, test_subject,train_t, train_activity, train_subject, test_all, train_all)

###############################STEP 2##############################
#Extracts the index of the columns for mean and std.
lb <- c(1,2) #keep first two lables
j <- 3 #start from the third label
int_lbs <- colnames(all) 
for (i in 3:length(int_lbs)){ ##search from third label onwards
 if(grepl("std", int_lbs[i]) | grepl("mean", int_lbs[i])){ 
     lb[j] <- i
     j=j+1
        }
}
#filter data to only include the mean and std columns
tidy_data <- all[,lb]

###############################STEP 4##############################
#Uses descriptive activity names to name the activities in the data set
f <- tidy_data[,2] == 1 
tidy_data[f,2] <- "WALKING"

f <- tidy_data[,2] == 2 
tidy_data[f,2] <- "WALKING_UPSTRAIRS"

f <- tidy_data[,2] == 3 
tidy_data[f,2] <- "WALKING_DOWNSTAIRS"

f <- tidy_data[,2] == 4 
tidy_data[f,2] <- "SITTING"

f <- tidy_data[,2] == 5 
tidy_data[f,2] <- "STANDING"

f <- tidy_data[,2] == 6
tidy_data[f,2] <- "LAYING"

colnames(tidy_data)[1] <- "subject_id"
colnames(tidy_data)[2] <- "activity"

###############################STEP 5##############################
#Appropriately labels the data set with descriptive variable names.
#remove special chars
lbls <- colnames(tidy_data)
y <- gsub("[\\()-]", "", lbls)

#remove typos
y <- gsub("BodyBody", "Body", y)

y <- gsub("^f", "frequencyDomain", y)
y <- gsub("^t", "timeDomain", y)
y <- gsub("Acc", "Accelerometer", y)
y <- gsub("Gyro", "Gyroscope", y)
y <- gsub("Mag", "Magnitude", y)
y <- gsub("Freq", "Frequency", y)
y <- gsub("mean", "Mean", y)
y <- gsub("std", "StandardDeviation", y)

colnames(tidy_data) <- y

###############################STEP 6##############################
#From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.


x <- tidy_data %>% group_by(subject_id,activity) %>% summarise_all(funs(mean))
