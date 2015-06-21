library('data.table')
library('reshape2')
# reading info data
setwd('UCI HAR Dataset')
activity_labels <- fread('activity_labels.txt')
features <- fread('features.txt')
# selected features
selected_features_id <- features[grepl('-mean\\(\\)$', features[[2]])][[1]]
selected_features_id <- append(selected_features_id, features[grepl('-std\\(\\)$', features[[2]])][[1]])
selected_features_id <- sort(selected_features_id)
selected_features <- features[selected_features_id][[2]]
# reading raw train data
setwd('train')
text <- gsub('  ', ' ', readLines('X_train.txt'), fixed=TRUE)
text <- gsub('^ ', '', text)
raw_train <- sapply(strsplit(text, ' '), as.double)
raw_train <- t(raw_train)
subject_train <- fread('subject_train.txt')
y_train <- fread('y_train.txt')
setwd('..')
# tyding train data
activity_type <- sapply(y_train, function(x){return(activity_labels[x][[2]])})
train <- data.frame(subject_train, activity_type)
colnames(train) <- c('subject_id', 'activity_type')
for(selected_feature_id in selected_features_id){
    feature_name <- features[selected_feature_id][[2]]
    train[[feature_name]] <- raw_train[,selected_feature_id]
}
# reading raw test data
setwd('test')
text <- gsub('  ', ' ', readLines('X_test.txt'), fixed=TRUE)
text <- gsub('^ ', '', text)
raw_test <- sapply(strsplit(text, ' '), as.double)
raw_test <- t(raw_test)
subject_test <- fread('subject_test.txt')
y_test <- fread('y_test.txt')
setwd('..')
# tyding test data
activity_type <- sapply(y_test, function(x){return(activity_labels[x][[2]])})
test <- data.frame(subject_test, activity_type)
colnames(test) <- c('subject_id', 'activity_type')
for(selected_feature_id in selected_features_id){
    feature_name <- features[selected_feature_id][[2]]
    test[[feature_name]] <- raw_test[,selected_feature_id]
}
# merging tyde train and test data
data <- merge(train, test, all=TRUE)
# getting new data
melted <- melt(data, id=c('subject_id', 'activity_type'), measure.vars=selected_features)
result <- dcast(melted, activity_type + subject_id ~ variable, mean)
# writing result
setwd('..')
write.table(result, 'out.txt', row.names=FALSE)
