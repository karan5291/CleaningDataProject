run_analysis <- function()

library(dplyr)
library(tidyr)
#Load the required data into R-variables
xtrain <- read.table("train/X_train.txt")
xtest <- read.table("test/X_test.txt")
ytrain <- read.table("train/y_train.txt")
ytest <- read.table("test/y_test.txt")
activities <- read.table("activity_labels.txt")
features <- read.table("features.txt")
subjecttrain <- read.csv("train/subject_train.txt", header = FALSE)
subjecttest <- read.csv("test/subject_test.txt",header=FALSE)

#COnvert variables into Dplyr tables
xtrain <- tbl_df(xtrain)
xtest <- tbl_df(xtest)
ytrain <- tbl_df(ytrain)
ytest <- tbl_df(ytest)
features <- tbl_df(features)
subjecttrain <- tbl_df(subjecttrain)
subjecttest <- tbl_df(subjecttest)
activities <- tbl_df(activities)

#Get vector of features containing mean() or std()
m_featuresindex <- c(grep("std\\(\\)", features$V2),grep("mean\\(\\)", features$V2))
m_features <- features[m_featuresindex,]

# get only the columns containing mean() and Std()
xtrain <- xtrain[,m_features$V1]
xtest <- xtest[,m_features$V1]

#rename col.names
names(xtrain) <- m_features$V2
names(xtest) <- m_features$V2

## Joining activity names by ytrain (activity_id for each observation)
ytrain <- inner_join(ytrain,activities, by = 'V1')
ytest <- inner_join(ytest,activities, by = 'V1')

#select only activity_lables and rename varaiable
ytrain <- select(ytrain, V2)
ytrain <- rename (ytrain, Act = V2)
ytest <- select(ytest, V2)
ytest <- rename (ytest, Act = V2)



#renaming Subject variable
subjecttrain <- rename(subjecttrain, Subject = V1)
subjecttest <- rename(subjecttest, Subject = V1)

##adding column with activity_lables and subject_number to meassurements
traindata <- cbind(xtrain,ytrain)
traindata <- cbind(traindata,subjecttrain)
testdata <- cbind(xtest,ytest)
testdata <- cbind(testdata,subjecttest)

#joining train and test data to one dataset
data <- rbind(traindata,testdata)

data <- gather(data, Meassure,Value, -Act,-Subject)

run_analysis <- summarize(group_by(data,Act,Subject,Meassure), mean(Value))




