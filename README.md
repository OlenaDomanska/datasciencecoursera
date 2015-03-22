# The scpirt works in such a way:

##1.Firstly we  import data for the train data set into R
 which contains such variables:

 
* An identifier of the subject who carried out the experiment, for that we read file "subject_train.txt"..
* Its activity label, for that we read file "y_train.txt". 
* A 561-feature vector with time and frequency domain variables, for that we read file "x_train.txt". 
* The estimated body acceleration, for that we read files "body_acc_x_train.txt", "body_acc_y_train.txt", "body_acc_z_train.txt".
* Triaxial Angular velocity from the gyroscope, for that we read files "body_gyro_x_train.txt", "body_gyro_y_train.txt", "body_gyro_z_train.txt".
* Triaxial acceleration from the accelerometer (total acceleration), for that we read files "total_acc_x_train.txt", "total_acc_y_train.txt", "total_acc_z_train.txt".

##2.Then we  import data for the test data set into R analogically.

## 3. create the train data set

DataTrain=data.frame()

## 4. create the test data set

DataTest=data.frame()

## 5.Merges the training and the test sets to create one data set

m=merge(DataTrain,DataTest,all=TRUE)

## 6.Extracts only the measurements on 
##the mean and standard deviation for each measurement

features=read.table("features.txt")
i=grep("mean",features[,2])
j=grep("std",features[,2])
k=c(i,j)
k1=k+2
k2=c(1,2,k1,564:1715)
m1=m[,k2]

## 7.Uses descriptive activity names to name the activities in the data set

activity_labels=read.table("activity_labels.txt")
m1[,2]=factor(m1[,2],labels=activity_labels[,2])

## 8.Appropriately labels the data set with descriptive variable names.
colnames(m1)[1]="subject"
colnames(m1)[2]="activity"
colnames(m1)[3:(length(k1)+2)]=paste("feature_vector",features[k1-2,2],sep="_")

colnames(m1)=gsub("-","_",colnames(m1),fixed = TRUE)
colnames(m1)=gsub("()","",colnames(m1),fixed = TRUE)


## 9.From the data set in step 8, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.


library(plyr)
subject_activity=paste(m1$subject,m1$activity,sep=" ")








