library(caret)
install.packages("ellipse")

data("iris")
dataset <- iris

?createDataPartition

validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

dim(dataset)
sapply(dataset, class)
head(dataset)

levels(dataset$Species)

percentange <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentange=percentange)

summary(dataset)

x <- dataset[,1:4]
y <- dataset[,5]

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)

print(fit.svm)

predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
