install.packages('caret', repos='http://cran.us.r-project.org')
library("caret")
library("klaR")
library(MASS)
install.packages("e1071")
train <- read.csv('./input/train.csv')
test  <- read.csv('./input/test.csv')
str(train)
summary(train)

?featurePlot
sum(is.na(train$Name))


featurePlot(x = train[, c(3, 5, 6, 8, 11)], y = train$Survived, plot = "pairs", auto.key=as.factor(train$Survived))
trainSel = subset(train, select = c('Survived', 'Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Embarked'))
testSel = subset(test, select = c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Embarked'))
head(trainSel)
head(testSel)

trainSel = subset(trainSel, Embarked != '')
trainSel$Embarked = droplevels(trainSel$Embarked, "")
testSel = subset(testSel, Embarked != '')

agemedian = median(train$Age, na.rm = TRUE)
trainSel$Age = replace(trainSel$Age, is.na(trainSel$Age), agemedian)
testSel$Age = replace(test$Age, is.na(test$Age), agemedian)

trainVars = trainSel[, c(2:7)]
objLabels = trainSel[, 1]
objLabels = as.factor(objLabels)
testVars = testgSel
testPsgIds = test[, 1]

inTrain = createDataPartition(objLabels, p = 0.75, list = FALSE)
forTrainVars = trainVars[inTrain,]
forTestVars = trainVars[-inTrain,]
forTrainLabels = objLabels[inTrain]
forTestLabels = objLabels[-inTrain]

set.seed(2345)
modeldt = train(y = forTrainLabels, x = forTrainVars, method = "rpart")

preddt = predict(modeldt, forTestVars)
confusionMatrix(forTestLabels, preddt)

modelnb = train(y = forTrainLabels, x= forTrainVars, method = "nb",)
prednb = predict(modelnb, forTestVars)
confusionMatrix(forTestLabels, prednb)

modelrf = train(y = forTrainLabels, x = forTrainVars, method = "rf",)

predrf = predict(modelrf, forTestVars)
confusionMatrix(forTestLabels, predrf)

set.seed(2345)

install.packages('Hmisc')