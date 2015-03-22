## download and unzip the data for the project
fileUrl<-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,dest="getdata-projectfiles-UCI HAR Dataset.zip", mode="wb")
unzip ("getdata-projectfiles-UCI HAR Dataset.zip", exdir = ".")


## import data for the train data set into R
subject_train=read.table("./train/subject_train.txt")
activity_label_train=read.table("./train/y_train.txt")
feature_vector_train=read.table("./train/x_train.txt")
body_acc_x_train=read.table("./train/Inertial Signals/body_acc_x_train.txt")
body_acc_y_train=read.table("./train/Inertial Signals/body_acc_y_train.txt")
body_acc_z_train=read.table("./train/Inertial Signals/body_acc_z_train.txt")
body_gyro_x_train=read.table("./train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_y_train=read.table("./train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_z_train=read.table("./train/Inertial Signals/body_gyro_z_train.txt")
total_acc_x_train=read.table("./train/Inertial Signals/total_acc_x_train.txt")
total_acc_y_train=read.table("./train/Inertial Signals/total_acc_y_train.txt")
total_acc_z_train=read.table("./train/Inertial Signals/total_acc_z_train.txt")

## import data for the test data set into R
subject_test=read.table("./test/subject_test.txt")
activity_label_test=read.table("./test/y_test.txt")
feature_vector_test=read.table("./test/x_test.txt")
body_acc_x_test=read.table("./test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test=read.table("./test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test=read.table("./test/Inertial Signals/body_acc_z_test.txt")
body_gyro_x_test=read.table("./test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_test=read.table("./test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_test=read.table("./test/Inertial Signals/body_gyro_z_test.txt")
total_acc_x_test=read.table("./test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_test=read.table("./test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_test=read.table("./test/Inertial Signals/total_acc_z_test.txt")

## create the train data set

DataTrain=data.frame(subject=subject_train,
activity_label=activity_label_train,feature_vector=feature_vector_train,
body_acc_x=body_acc_x_train,body_acc_y=body_acc_y_train,body_acc_z=body_acc_z_train,
body_gyro_x=body_gyro_x_train,body_gyro_y=body_gyro_y_train,body_gyro_z=body_gyro_z_train,
total_acc_x=total_acc_x_train,total_acc_y=total_acc_y_train,total_acc_z=total_acc_z_train)

## create the test data set

DataTest=data.frame(subject=subject_test,
activity_label=activity_label_test,feature_vector=feature_vector_test,
body_acc_x=body_acc_x_test,body_acc_y=body_acc_y_test,body_acc_z=body_acc_z_test,
body_gyro_x=body_gyro_x_test,body_gyro_y=body_gyro_y_test,body_gyro_z=body_gyro_z_test,
total_acc_x=total_acc_x_test,total_acc_y=total_acc_y_test,total_acc_z=total_acc_z_test)

## 1.Merges the training and the test sets to create one data set

m=merge(DataTrain,DataTest,all=TRUE)

## 2.Extracts only the measurements on 
##the mean and standard deviation for each measurement

features=read.table("features.txt")
i=grep("mean",features[,2])
j=grep("std",features[,2])
k=c(i,j)
k1=k+2
k2=c(1,2,k1,564:1715)
m1=m[,k2]

## 3.Uses descriptive activity names to name the activities in the data set

activity_labels=read.table("activity_labels.txt")
m1[,2]=factor(m1[,2],labels=activity_labels[,2])

## 4.Appropriately labels the data set with descriptive variable names.
colnames(m1)[1]="subject"
colnames(m1)[2]="activity"
colnames(m1)[3:(length(k1)+2)]=paste("feature_vector",features[k1-2,2],sep="_")

colnames(m1)=gsub("-","_",colnames(m1),fixed = TRUE)
colnames(m1)=gsub("()","",colnames(m1),fixed = TRUE)


## 5.From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.


library(plyr)
subject_activity=paste(m1$subject,m1$activity,sep=" ")

mAverage=mutate(m1,subject_activity)
mAverage=mAverage[,-(1:2)]
 mAverage2=mAverage %>% group_by(subject_activity) %>% summarise_each(funs(mean))

u=unlist(strsplit(mAverage2$subject_activity," ", fixed = TRUE))
u=split(u,1:2)
 mAverage3=cbind(activity=u$"2",mAverage2)
mAverage3=cbind(subject=u$"1",mAverage3)
mAverage3=mAverage3[,-3]

