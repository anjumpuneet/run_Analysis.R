run_Analysis <- function() {

##run_Analysis.R

##This Function will load the library reshape2 and assumes that its already installed.
##The working Directory has been set to the root of extracted data folder 


library(reshape2)

##Import Data form Root folder into R variables using Read.table

train.x <- read.table("train/X_train.txt")
test.x <- read.table("test/X_test.txt")
features <- read.table("features.txt")
train.y <- read.table("train/y_train.txt")
Subtrain <- read.table("train/subject_train.txt")
test.y <- read.table("test/y_test.txt")
Subtest <- read.table("test/subject_test.txt")


##Label the data sets Imported above with activity names.
##Clean up colNames and Name columns of data frames

colNames <- gsub("[[:punct:]]","", features$V2, ignore.case = FALSE, perl = FALSE)
colnames(train.x) <- colNames
colnames(Subtrain) <- "Subject"
colnames(train.y) <- "Activity"
colnames(test.x) <- colNames
colnames(Subtest) <- "Subject"
colnames(test.y) <- "Activity"


##Combine test and train data with subjects into single dataset   
##Combine testsComplete and trainComplete data into one data set

testsComplete <- cbind(Subtest, test.y,test.x)
trainComplete <- cbind(Subtrain,train.y,train.x)
allData <- rbind(trainComplete,testsComplete)


##Extract the mean and standard deviation for each measurement. 
##Extract Subject and Activity data
## Load this into a new data set

NewData <- allData[, grep("mean|std|Subject|Activity", names(allData))]


##Attach descriptive activity names to name the activities in the New Data data set

labels <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
NewData$Activity <- labels[NewData$Activity]



##Creates a second, independent tidy data set with the average of each 
##variable for each activity and each subject.

melted = melt(NewData, id.var = c("Subject", "Activity"))
means = dcast(melted , Subject + Activity ~ variable,mean)


## Write tidy data to space separated text file

write.table(means, file="MTidyData.txt", sep = " ")
}
