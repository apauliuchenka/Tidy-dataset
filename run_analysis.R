getTidyDataset <- function()
{
        alldata <- bindTestTrainDatasets()
        alldata <- nameDataset(alldata)
        tidy <- selectStdMeanColumns(alldata)
        tidy <- nameActivities(tidy)
        result <- calcMeanValues(tidy)
        saveResultDataset(result)
}

bindTestTrainDatasets <- function()
{
        ##read test files and merge in one dataset using adding columns
        testx <- read.table("./test/X_test.txt")
        testy <- read.table("./test/Y_test.txt")
        tests <- read.table("./test/subject_test.txt")
        testdata <- cbind(tests, testy, testx)          ## dimension 2947x563
        
        ##read train files and merge in one dataset using adding columns
        trainx <- read.table("./train/X_train.txt")
        trainy <- read.table("./train/Y_train.txt")
        trains <- read.table("./train/subject_train.txt")
        traindata <- cbind(trains, trainy, trainx)      ## dimension 7352x563
        
        ## merge test and train datasets in one using adding rows
        rbind(testdata,traindata)                       ## dimension 10299x563
}

nameDataset <- function(alldata)
{
        features <- read.table("./features.txt", stringsAsFactors=FALSE)
        ##remove unnessesary characters like () from column names
        col <- gsub("[[:punct:]]{2}", "", make.names(features$V2)) 
        
        names(alldata) <- c("subjectId", "activityId", col)
        alldata
}

selectStdMeanColumns <- function(alldata)
{
        col <- grep("mean|std", names(alldata), value = TRUE)
        tidy <- alldata[col]
        
        ##adding activity and subject columns to std/mean dataset
        cbind(alldata[1], alldata[2], tidy)
}

nameActivities <- function(tidy)
{
        act <- read.table("./activity_labels.txt", stringsAsFactors=FALSE) 
        ## merge act and tidy dataframes and remove unnecessary column
        tidy <- merge(act, tidy, by.x = "V1", by.y="activityId")
        tidy <- tidy[,-1]
        names(tidy)[1] <- "activity"
        tidy
}

calcMeanValues <- function(tidy)
{
        result <- aggregate(tidy[,3:81], by=list(tidy$activity, tidy$subjectId), mean)
        names(result)[1] <- "activity"       
        names(result)[2] <- "subjectId"
        result
}

saveResultDataset <- function(result)
{
        write.table(result, file="tidyDataset.txt",row.name=FALSE)       
}