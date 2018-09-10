library(readr)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(ROCR)
set.seed(101)

titanic3 <- "https://goo.gl/At238b" %>%
  read_csv %>% #read in data
  select(survived, embarked, sex, sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked), sex = factor(sex))

head(titanic3)

#figure out pairwise plot to see who lived and who died
pairs(titanic3) #this aint right

#split data
.data <- c("training", "test") %>%
  sample(nrow(titanic3), replace = T) %>%
  split(titanic3, .)

#recursive partitioning is implemented in rpart package
rtree_fit <- rpart(survived ~ ., .data$training)
rpart.plot(rtree_fit)

#conditional partitioning is implemented in the ctree method
tree_fit <- ctree(survived ~ ., data = .data$training)
#plot to visualize
plot(tree_fit)

#use ROCR package to visualize ROC curve and compare methods
#for comparison, we compare the decision tree, conditional tree, and logistic regression
tree_roc <- tree_fit %>%
  predict(newdata = .data$test) %>%
  prediction(.data$test$survived) %>%
  performance("tpr", "fpr")
plot(tree_roc) #this aint right