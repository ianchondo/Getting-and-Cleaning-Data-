# Obtain the data and download the file from this url
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "/Users/inesanchondo/Desktop/Coursera/GettingandCleaningData-Ines/Project/
           Dataset.zip", method = "curl")

# 1 Reads the data.

TraindataX <- read.table("./UCI HAR Dataset/train/X_train.txt") 
[1] 7352  561
TraindataY <- read.table("./UCI HAR Dataset/train/y_train.txt")
table(TraindataY)
TraindataY
1    2    3    4    5    6 
1226 1073  986 1286 1374 1407 
Trainsubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
table(Trainsubject)
Trainsubject
1   3   5   6   7   8  11  14  15  16  17  19  21  22  23  25  26  27  28  29  30 
347 341 302 325 308 281 316 323 328 366 368 360 408 321 372 409 392 376 382 344 383
TestdataX <- read.table("./UCI HAR Dataset/test/X_test.txt")
[1] 2947  561
TestdataY <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table (TestdataY)
TestdataY
1   2   3   4   5   6 
496 471 420 491 532 537 
Testsubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
table (Testsubject)
Testsubject
2   4   9  10  12  13  18  20  24 
302 317 288 294 320 327 364 354 381

#2. Merges the data
mergeData <- rbind(TraindataX, TestdataX)
dim(mergeData)
[1] 10299   561
mergeLabel <- rbind(TraindataY, TestdataY)
dim(mergeLabel)

mergeSubject <- rbind(Trainsubject, Testsubject)
dim(merge(Subject)
[1] 10299     1   
#3. Reads features.txt and calculates means and standard deviations for each measurement.
features <- read.table("./UCI HAR Dataset/features.txt")

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
mergeData <- mergeData[, meanStdIndices]


#4. Labels each variable.
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity [, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[mergeLabel[, 1], 2]
mergeLabel[, 1] <- activityLabel
names(mergeLabel) <- "activity"
names(mergeSubject) <- "subject"

#5 Creates at tidy data set
cleanedData <- cbind(mergeSubject, mergeLabel, mergeData)
dim(cleanedData)
[1] 10299    68
subjectTidy <- length(table(mergeSubject)) 
activityTidy <- dim(activity)[1] 
columnTidy <- dim(cleanedData)[2]
table <- matrix(NA, nrow=subjectTidy*activityTidy, ncol=columnTidy) 
table <- as.data.frame(table)
colnames(table) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectTidy) {
        for(j in 1:activityTidy) {
                table[row, 1] <- sort(unique(mergeSubject)[, 1])[i]
                table[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                table[row, 3:columnTidy] <- colMeans(cleanedData[bool1&bool2, 3:columnTidy])
                row <- row + 1
        }
}
head(table)
write.table(table, "tidyData.txt") 