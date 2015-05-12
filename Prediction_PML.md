---
title: "Practical Machine Learning"
author: "Olena Domanska"
date: "Monday, April 27, 2015"
output: html_document
---
Predicting type of movement


The goal of this exercise is to help identify a model that can describe 
(or predict) the type of movement being performed based on the same 
acceleromter data used to identify general movement.

Approach

As the accuracy of the results is key I chose a random forest model with 1000 trees.

Data Wrangling

Basic idea is to remove unecessary or irrelevant variables to create a better 
initial data set. Then create a new training and testing set based on the 
initial set provided.
In essense there are thee movements calculated at the accelerometer and gyro:
a roll, pitch and yaw that are measured at a belt, arm, forearm and dumbbell 
location.


## Loading and preprocessing the data

Let's first download the data for the project



```r
if (!file.exists("./pml-training.csv")) {
  fileUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(fileUrl, dest = "pml-training.csv", mode = "wb")
  }
if (!file.exists("./pml-testing.csv")) {
  fileUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(fileUrl, dest = "pml-testing.csv", mode = "wb")
  }
```

Now load the data


```r
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
```

Identified list of variables per the documentation that are 
statistical descriptors of the actual measurements captured

Since the variable names vary I found it easier to identify them based 
on their first 4 characters

```r
set.seed(1)
keep.vars <- c("roll","pitc","yaw_", "gyro","acce","clas")
training.trim <- training[,substr(colnames(training),1,4) %in% keep.vars ]
training.trim$classe <- as.factor(training.trim$classe)
```

Then cut the set into a training and test set to prevent 
overfitting the model


```r
library(caret)
library(ggplot2)
library(dplyr)
library(randomForest)
inTrain <- createDataPartition(training.trim$classe, p = 0.5, list = FALSE)
new.training <- training.trim[inTrain, ]
new.testing <- training.trim[-inTrain, ]
dim(new.training); dim(new.testing)
```

```
## [1] 9812   37
```

```
## [1] 9810   37
```

Model, Errors and Predictions - randomForest model

```r
rfFit <- randomForest(classe ~ ., data = new.training, 
                       importance = TRUE, ntree = 1000)
```
Perform predictions

```r
predTrain <- predict(rfFit, newdata = new.training)
new.training$predRight <- predTrain == new.training$classe
table(predTrain, new.training$classe)
```

```
##          
## predTrain    A    B    C    D    E
##         A 2790    0    0    0    0
##         B    0 1899    0    0    0
##         C    0    0 1711    0    0
##         D    0    0    0 1608    0
##         E    0    0    0    0 1804
```

Calculate the in-sample error rate

```r
predTrainAccuracy <- sum(ifelse(new.training$predRight == TRUE,1,0)) / nrow(new.training)
1-predTrainAccuracy
```

```
## [1] 0
```
Now calculate the out of sample error rate on our predictions from  our testing set

```r
predTest <- predict(rfFit, newdata = new.testing)
new.testing$predRight <- predTest == new.testing$classe
table(predTest, new.testing$classe)
```

```
##         
## predTest    A    B    C    D    E
##        A 2780   17    0    0    0
##        B    7 1877   10    0    0
##        C    1    2 1692   20    3
##        D    1    0    9 1583   11
##        E    1    2    0    5 1789
```

Calculate the out of sample error rate

```r
predTestAccuracy <- sum(ifelse(new.testing$predRight == TRUE,1,0)) / nrow(new.testing)
1-predTestAccuracy
```

```
## [1] 0.009072375
```
Let's take a look at identifying those values that missed the mark on our test set I can see through importance that the accel values are some of the highest valued group, let's view them accordingly: at the accel at the belt level we can see some of our missses not aligning with the other groups

```r
qplot(accel_belt_x, accel_belt_y, data = new.testing, colour = predRight)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

At the arm the pattern is less identifieable

```r
qplot(accel_arm_x, accel_arm_y, data = new.testing, colour = predRight)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

We can also see some of these same failures show up in a smaller subset of the belt movements

```r
qplot(roll_belt, yaw_belt, data = new.testing, colour = predRight)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

### Cross Validation

Since random forest was used a cross validation is already computed internally with the oob (out-of-bag) error estimate which in this case is 0.

### Conclusion

We can use the model provided to predict with an estimated accuracy (based on our test set) of over 99%, more validation needs to be accomplished to "operation-alize" any of this information but our per our current method the confidnece is there.


library(knitr)
knit2html("Prediction_PML.Rmd")
browseURL("Prediction_PML.html")
