library(dplyr)
train <- read.csv('./input/censusData_train.csv')
test  <- read.csv('./input/censusData_test.csv')

summary(train)

validation_index <- createDataPartition(train$Salarary, p=0.80, list=FALSE)
validation <- train[-validation_index,]
train <- train[validation_index,]

dim(train)
sapply(train, class)
head(train)

levels(train$Salarary)

percentange <- prop.table(table(train$Salarary)) * 100
cbind(freq=table(train$Salarary), percentange=percentange)
train$Sex <- c(as.numeric(train$Sex))
train$Education <- c(as.numeric(train$Education))
train$Marital.status <- c(as.numeric(train$Marital.status))
train$Occupation <- c(as.numeric(train$Occupation))
train$Native.country <- c(as.numeric(train$Native.country))
train$Race <- c(as.numeric(train$Race))
train$Relationship <- c(as.numeric(train$Relationship))
train$Workclass <- c(as.numeric(train$Workclass))

head(train)

x <- train[,1:14]
y <- train[,15]

par(mfrow=c(1,14))
for(i in 1:14) {
  boxplot(x[,i], main=names(train)[i])
}


head(x)

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

set.seed(7)
fit.lda <- train(Salarary~., data=train, method="lda", metric=metric, trControl=control)

predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Salarary)