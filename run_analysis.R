 y_test <- read.table("y_test.txt")
  names(y_test) <- "activityID"  
  
  subject_test <- read.table("subject_test.txt") 
  names(subject_test) <- "subjectID"  
  
  X_test <- read.table("X_test.txt")     
  features <- read.table("features.txt")  
  names(X_test) <- features[, 2] 
  
  dataset_test <- data.frame(subject_test, y_test, X_test) 

  y_train <- read.table("y_train.txt") 
  names(y_train) <- "activityID"  
  
  subject_train <- read.table("subject_train.txt") 
  names(subject_train) <- "subjectID"  
  
  X_train <- read.table("X_train.txt")      
  ## features <- read.table("features.txt")  
  names(X_train) <- features[, 2] 

  dataset_train <- data.frame(subject_train, y_train, X_train) 
  
  ##1.Merges the training and the test sets to create one data set.
  dataset <- rbind(dataset_test, dataset_train)
  #dataset
  
  ##Extracts only the measurements on the mean and standard deviation for each measurement. 
  mean_pos <- grep('mean', colnames) + 2   
  std_pos <- grep('std', colnames) + 2
  dataset_extract <- data.frame(dataset[, 1], dataset[, 2])
  for(i in 1 : length(mean_pos))
  {
    dataset_extract <-data.frame(dataset_extract, dataset[, mean_pos[i]])
  }
  for(i in 1 : length(std_pos))
  {
    dataset_extract <-data.frame(dataset_extract, dataset[, std_pos[i]])
  }
  #dataset_extract
  
  ##Appropriately labels the data set with descriptive variable names. 
  names(dataset_extract) <- c("subjectID", "activityID", "tBAmean1_X", "tBAmean2_Y", "tBAmean3_Z", "tBAstd4_X", 
                              "tBAstd5_Y","tBAstd6_Z",  "tGAmean41_X", "tGAmean42_Y", "tGAmean43_Z", "tGAstd44_X", 
                              "tGAstd45_Y", "tGAstd46_Z", "tBAJmean81_X",  "tBAJmean82_Y", "tBAJmean83_Z",  "tBAJstd84_X", 
                              "tBAJstd85_Y", 
                              "tBAJstd86_Z", 
                              "tBGmean121_X", 
                              "tBGmean122_Y", 
                              "tBGmean123_Z", 
                              "tBGstd124_X", 
                              "tBGstd125_Y", 
                              "tBGstd126_Z", 
                              "tBGJmean161_X", 
                              "tBGJmean162_Y", 
                              "tBGJmean163_Z", 
                              "tBGJstd164_X", 
                              "tBGJstd165_Y", 
                              "tBGJstd166_Z", 
                              "tBAMmean201", 
                              "tBAMstd202", 
                              "tGAMmean214", 
                              "tGAMstd215", 
                              "tBAJMmean227", 
                              "tBAJMstd228", 
                              "tBGMmean240", 
                              "tBGMstd241", 
                              "tBGJMmean253", 
                              "tBGJMstd254", 
                              "fBAmean266_X", 
                              "fBAmean267_Y", 
                              "fBAmean268_Z", 
                              "fBAstd269_X", 
                              "fBAstd270_Y", 
                              "fBAstd271_Z", 
                              "fBAmeanF294_X", 
                              "fBAmeanF295_Y", 
                              "fBAmeanF296_Z", 
                              "fBAJmean345_X", 
                              "fBAJmean346_Y", 
                              "fBAJmean347_Z", 
                              "fBAJstd348_X", 
                              "fBAJstd349_Y", 
                              "fBAJstd350_Z", 
                              "fBAJmeanF373_X", 
                              "fBAJmeanF374_Y", 
                              "fBAJmeanF375_Z", 
                              "fBGmean424_X", 
                              "fBGmean425_Y", 
                              "fBGmean426_Z", 
                              "fBGstd427_X", 
                              "fBGstd428_Y", 
                              "fBGstd429_Z", 
                              "fBGmeanF452_X", 
                              "fBGmeanF453_Y", 
                              "fBGmeanF454_Z", 
                              "fBAMmean503", 
                              "fBAMstd504", 
                              "fBAMmeanF513", 
                              "fBBAJMmean516", 
                              "fBBAJMstd517", 
                              "fBBAJMmeanF526", 
                              "fBBGMmean529", 
                              "fBBGMstd530", 
                              "fBBGMmeanF539",
                              "fBBGJMmean542", 
                              "fBBGJMstd543", 
                              "fBBGJMmeanF552")
  ##Uses descriptive activity names to name the activities in the data set
  activity_labels <- read.table("activity_labels.txt")
  names(activity_labels) <- c("activityID", "activityID_name")
  dataset_extract_merge <- merge(dataset_extract, activity_labels, by = "activityID")
  
  ##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  compute_mean_data <- aggregate(dataset_extract_merge[,3:81], by = list(dataset_extract_merge$subjectID, dataset_extract_merge$activityID_name), FUN = mean)
  colname_fin <- names(compute_mean_data)
  colname_fin[1] <- "subjectID"
  colname_fin[2] <- "activityName"
  names(compute_mean_data) <- colname_fin
  #compute_mean_data
  write.table(compute_mean_data,"final_result.txt",row.names=FALSE)
