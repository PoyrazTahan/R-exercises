# Import training and testing datafiles
require(lattice)
require(ggplot2)

W6_Car$X1 <- factor(W6_Car$X1)
W6_Car$X2 <- factor(W6_Car$X2)
W6_Car$X5 <- factor(W6_Car$X5)
W6_Car$X6 <- factor(W6_Car$X6)
W6_Car$X7 <- factor(W6_Car$X7)

cols <- character(nrow(W6_Car))
cols[] <- "black"

cols[W6_Car$X7 == "acc"] <- "blue"
cols[W6_Car$X7 == "good"] <- "red"
cols[W6_Car$X7 == "unacc"] <- "green"
pairs(W6_Car[1:6],col=cols)

plot(jitter(W6_Car$X3) ~ jitter(W6_Car$X4), pch = 15,col=cols)

plot(jitter(as.integer(W6_Car$X1)) ~ jitter(as.integer(W6_Car$X2)), pch = 15,col=alpha(cols, 0.5))

# Install and load required packages for fancy decision tree plotting
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
install.packages("caret")
library(caret)

options(digits=2)

fit1 <- rpart(X7 ~ . , data=train, method="class",
              control=rpart.control(minsplit=1))
rpart.plot(fit1)


# Now let's make predictions
predicttrain1 <- predict(fit1, train, type = "class")
confusionMatrix(predicttrain1,train$X7)
prediction1 <- predict(fit1, test, type = "class")
confusionMatrix(prediction1,test$X7)


#Repeat with minsplit = 100



#Repeat with smaller training set
set.seed(1)
train_s <- train[sample(1:nrow(train), 200, replace=FALSE),]

fits <- rpart(X7 ~ . , data=train_s, method="class",
              control=rpart.control(minsplit=1))
rpart.plot(fits)

predicttrains <- predict(fits, train_s, type = "class")
confusionMatrix(predicttrains,train_s$X7)
predictions <- predict(fits, test, type = "class")
confusionMatrix(predictions,test$X7)