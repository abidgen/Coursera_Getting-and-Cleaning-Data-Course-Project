
#1. Merges the training and the test sets to create one data set
    #a. Download zip file and unzip it

URL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "data.zip", mode = "wb")
unzip("data.zip")

    #b. Assigns raw data into variables

activity_labels<- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code","activity"))
feature<- read.table("UCI HAR Dataset/features.txt", col.names = c("n","feature_name"))
subject_test<- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test<- read.table("UCI HAR Dataset/test/X_test.txt")
y_test<- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <-read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train<- read.table("UCI HAR Dataset/train/X_train.txt")
y_train<- read.table("UCI HAR Dataset/train/y_train.txt")

    #c. Joins raw data into one complete dataset

library(dplyr)
feature<- feature %>% 
    mutate("feature_labels"= paste(feature$n,feature$feature_name, sep ="_" ))
activity_measurement<- rbind(x_train,x_test)
names(activity_measurement)<- feature$feature_labels

subject_identifier <- rbind(subject_train,subject_test) 

activity_identifier <- rbind(y_train,y_test) %>% 
    rename("code" = V1)

complete_dataset<- cbind(subject_identifier,activity_identifier,activity_measurement)



#2. Extracts only the measurements on the mean and standard deviation for each measurement
filtered_data<- complete_dataset %>% 
    select(subject, code, contains("mean"),contains("std"))
    

#3. Uses descriptive activity names to name the activities in the data set
tidy_data<- filtered_data %>% 
    mutate(code =  activity_labels[code,2]) %>%
    rename(activity=code)%>%
    arrange(subject,activity)
    
#4. Appropriately labels the data set with descriptive variable names
names(tidy_data)<-  gsub("(^[0-9]+_)+t","\\1Time",names(tidy_data))
names(tidy_data)<-  gsub("(^[0-9]+_)+f","\\1Frequency",names(tidy_data))
names(tidy_data)<-  gsub("Acc","Accelerometer",names(tidy_data))
names(tidy_data)<-  gsub("Gyro","Gyroscope",names(tidy_data))
names(tidy_data)<-  gsub("Mag","Magnitude",names(tidy_data))
names(tidy_data)<-  gsub("BodyBody","Body",names(tidy_data))
names(tidy_data)<-  gsub("std+\\(+\\)","STD",names(tidy_data))
names(tidy_data)<-  gsub("mean+\\(+\\)","Mean",names(tidy_data))
names(tidy_data)<-  gsub("meanFreq+\\(+\\)","MeanFrequrncy",names(tidy_data))


write.table(tidy_data, "tidy_data.txt", row.names = FALSE)

#5. Creates a second, independent tidy data set with the average of each variable
# for each activity and each subject.
final_data<- tidy_data %>% group_by(subject, activity)%>% summarise_all(mean)

write.table(final_data, "final_data.txt", row.names = FALSE)


