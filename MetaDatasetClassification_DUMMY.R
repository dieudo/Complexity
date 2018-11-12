#install.packages("caret")
#install.packages("caret", dependencies=c("Depends", "Suggests"))

#Dataset creation
Al=sample(c("SVM","ANN","Knn","GLM","DT"),100,replace=T)
df=cbind(Al,runif(100),runif(100),runif(100))
df
head(df)
write.csv(df,file="df.csv")

DummyChar <- read.csv("~/DummyChar.csv")
#My Meta Dataset
dataset <- DummyChar
dataset
library(caret)
validation_index <- createDataPartition(dataset$Al, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# list the levels for the class
levels(dataset$Al)

# summarize the class distribution
percentage <- prop.table(table(dataset$Al)) * 100
cbind(freq=table(dataset$Al), percentage=percentage)


# split input and output
x <- dataset[,3:5]
y <- dataset[,2]
# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:3) {
  boxplot(x[i], main=names(x)[i])
}

# barplot for class breakdown
plot(y)
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(Al~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Al~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Al~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Al~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Al~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.rf)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions, validation$Al)

#Al=sample(c("SVM","ANN","Knn","GLM","DT"),100,replace=T)

