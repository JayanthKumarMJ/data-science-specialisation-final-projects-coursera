library(dtplyr)

## merges two files into one, and names it as <filename>_merged.csv
mergeFiles <- function(file1, file2, destFile, colNames = NULL){
    if(file.exists(file1) && file.exists(file2)){
        df1 <- fread(input = file1)
        df2 <- fread(input = file2)
        mergedDf <- rbind(df1,df2)
        rm(df1,df2)
        if(!is.null(colNames)){
            colnames(mergedDf) <- colNames
        }
        write.csv(x = mergedDf,file = destFile)
        rm(mergedDf)
    }
}

## creates a directory if it doesn't exist
createDirectory <- function(dir) {
    if(!file.exists(dir)){
        dir.create(dir)
    }
}

## takes root path of train data set and merge path
## assumption root path is train data set path and test set is ../test.
## hence it take the path and replaces train by test to create test data set path.
mergeData <- function(rootPath , mergedPath) {
    createDirectory(dir = mergedPath)
    listOfFilesToBeMerged <- getFileList(rootPath)
    lapply(listOfFilesToBeMerged, FUN = domerge, rootPath, mergedPath)
}

## for a given file, builds the test data test path and calls the mergeFiles function
domerge <- function(fileName, rootPath, mergedPath){
    otherFilePath <- gsub(pattern = "train", replacement = "test", 
                          paste(rootPath,fileName,sep = "/"))
    destFileName <- gsub(pattern = "train", replacement = "merged", fileName)
    createDirectoryStructure(destFileName,mergedPath)
    destFilePath <- paste(mergedPath,destFileName, sep = "/")
    filePath <- paste(rootPath,fileName,sep = "/")
    mergeFiles(file1 = filePath, file2 = otherFilePath, destFile = destFilePath)
}

getFileList <- function(path){
    list.files(path = path,recursive = TRUE)
}

## creates a directory tree, ignore the last path assuming it to be a file name.
createDirectoryStructure <- function(relPath, fullPath){
    list <- strsplit(relPath, split = "/")
    vec <- list[[1]]
    len <- length(vec)
    if(len>1){
        dir <- paste(vec[1:len-1], collapse = "/")
        dir <- paste(fullPath,dir, sep = "/")
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
}

## reads features.txt path of which will be parameter and returns the second column
getColumnName <- function(listOfLabelsFile){
    labelsDF <- fread(input = listOfLabelsFile)
    names(labelsDF) <- c("sl", "colNames")
    labelsDF[,colNames]
}

mergePath <- "git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/merged"
trainDataSetPath <- "git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/train"
featuresListFile <- "git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/features.txt"
activitiesListFile <- "git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/activity_labels.txt"
mergedActivitiesListFile <- "git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/merged/y_merged.txt"
subjectsFilePath <- "git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/merged/subject_merged.txt"
mergeData(trainDataSetPath,mergePath)

#2nd question
colNames <- getColumnName(featuresListFile)
l <- grepl(pattern = "(mean|std)\\(\\)",x = colNames)
colIndex <- which(colNames %in% colNames[l])
#ignoring serial number column
onlyMeanAndSd<- fread("git-hub/data-science-specialisation-final-projects-coursera/cleaning-data-assignment/UCI HAR Dataset/merged/X_merged.txt", 
                      select = colIndex+1)
colnames(onlyMeanAndSd) <- colNames[l]

#3rd question & 4th question
activities <- fread(activitiesListFile, col.names = c("id", "name"))
mergedActivitiesList <- fread(mergedActivitiesListFile, col.names = c("sl","id"))
descriptiveActivitydf <- merge(activities, mergedActivitiesList, by = "id")
descriptiveActivitydf <- descriptiveActivitydf[, sl := as.numeric(sl)]
descriptiveActivitydf <- descriptiveActivitydf[order(sl)]
withActivityName <- cbind(descriptiveActivitydf[,name],onlyMeanAndSd)
colnames(withActivityName)[1] <- "activityname"
rm(activities,mergedActivitiesList,descriptiveActivitydf)

#5th question.
meansForEachSubForEachAct <- fread(input = subjectsFilePath, select = 2, col.names = "subject") %>% 
    cbind(withActivityName) %>% 
    group_by(subject,activityname) %>% 
    summarise_each(funs(mean)) %>%
    arrange(subject,activityname)
write.table(meansForEachSubForEachAct, file = paste(mergePath,"summary.txt",sep = "/"), 
            append = FALSE, quote = FALSE)