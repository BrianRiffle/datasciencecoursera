## Capurtes the working directory
working <- getwd()
setwd("UCI HAR Dataset")

## Reads the feature labels and the names of activities
labels <- read.table("features.txt")
activites <- read.table("activity_labels.txt")

##Changes to the test sub diretory
setwd("test")

## Reads the data in the test sub directoy
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

## Adds column names using the feature labels read above
colnames(x_test) <- labels[,2]
colnames(y_test) <- "Activity"
colnames(subject_test) <- "Subject"

## Combinds the data from the three files read above into one data frame
test_data <- cbind(subject_test,y_test,x_test)

## Set directory to the train sub directory
setwd(working)
setwd("UCI HAR Dataset/train")

## Reads the data in the train sub directoy
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

## Adds column names using the feature labels read above
colnames(x_train) <- labels[,2]
colnames(y_train) <- "Activity"
colnames(subject_train) <- "Subject"

## Combinds the data from the three files read above into one data frame
train_data <- cbind(subject_train,y_train,x_train)

## Return to working directory
setwd(working)
setwd("UCI HAR Dataset")

## Combined the test and training data sets

data <- rbind(test_data, train_data)

## Determine which columns to keep
keep <- grep("mean()", labels$V2)
keep2 <- grep("std()", labels$V2)
keeper <- c(keep,keep2) + 2
keeplist <- c(1,2,keeper)
final <- sort(keeplist)

## Apply list to data
subdata <- data[,final]

## Replace activity codes with desciptive names
for (i in 1:nrow(subdata)) { 
        if (subdata[i,2] == 1) {subdata[i,2] <- "Walking"}
        if (subdata[i,2] == 2) {subdata[i,2] <- "Walking_Upstairs"}
        if (subdata[i,2] == 3) {subdata[i,2] <- "Walking_Downstairs"}
        if (subdata[i,2] == 4) {subdata[i,2] <- "Sitting"}
        if (subdata[i,2] == 5) {subdata[i,2] <- "Standing"}
        if (subdata[i,2] == 6) {subdata[i,2] <- "Laying"}
}

## Check for NA's
good <- nrow(subdata[(complete.cases(subdata)),])

## Create summary table for activity and subject
avgsubdata <- aggregate(subdata[,c(-1,-2)], list("subjectID"= subdata$Subject,"activity"=subdata$Activity),mean)

## Create text file from the data frame created above in the working directory
setwd(working)
write.table(avgsubdata,"meandata.txt",row.name=FALSE)



