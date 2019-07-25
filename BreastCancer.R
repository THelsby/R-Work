library(class)
library(ggplot2)

dataset <- read.csv('./input/data.csv')

dim(dataset)

dataset_NoID <- dataset[,-1]


featureScaling <- function(x) {
  ((x - min(x)) / (max(x) - min(x)))
}

dataset_Normalised <- as.data.frame(lapply(dataset_NoID[,-1], featureScaling))

dataset_Normalised <- cbind(dataset_Normalised, diagnosis=dataset_NoID[,1])


validation_index <- createDataPartition(dataset_NoID$diagnosis, p=0.80, list=FALSE)

dataset_Training <- dataset_Normalised[validation_index,]

dataset_Test <- dataset_Normalised[-validation_index,]

dataset_Training_diagnosis <- dataset_Normalised[validation_index, 30]

dataset_Testing_diagnosis <- dataset_Normalised[-validation_index, 30]

head(dataset_Training_diagnosis)


k_Value <- floor(sqrt(length(dataset_Training[,1])))
control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"

sapply(dataset_Training, class)

set.seed(7)
fit.knn <- train(diagnosis~., data=dataset_Normalised, method="knn", metric=metric, trControl=control)

#dataset_Prediction <- knn(dataset_Training, dataset_Test, dataset_Training_diagnosis, metric = metric, k=k_Value)

predictions <- predict(fit.knn, dataset_Test)

head(predictions)
head(dataset_Test$diagnosis)

confusionMatrix(predictions, dataset_Test$diagnosis)

#head(dataset_Prediction)
#table(dataset_Prediction, dataset_Testing_diagnosis)