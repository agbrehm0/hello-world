library(caret)
data(iris)
dataset <- iris

##load data
#80% of rows for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
#set 20% of data for validation
validation <- dataset[-validation_index,]
#use remaining 80% of data for training and testing the models
dataset <- dataset[validation_index,]

##summarize data set
#dimensions of dataset
dim(dataset)
#types of attributes
sapply(dataset, class)
#peak at data
head(dataset)
#what are the possible classifications?
levels(dataset$Species)
#distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)
#summarize
summary(dataset)

##visualize
#split input and output
x <- dataset[,1:4]
y <- dataset[,5]
#boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
	boxplot(x[,i], main = names(iris[i]))
}
#bar plot for class breakdown
plot(y)
#scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
#box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")
#density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

##evaluate some algorithms
#run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
#MODELS
#a) linear algorithms
set.seed(101)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

#b) non linear algorithms
#CART
set.seed(101)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
#knn
set.seed(101)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

#c) advanced algorithms
#svm
set.seed(101)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
#random forest
set.seed(101)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#selecting best model
#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
#compare accuracy of models
dotplot(results)

#from the dot plot we see lda has highest accuracy so summarize that
print(fit.lda)

##Make Predictions
#estimate skill of lda on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)