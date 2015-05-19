#Reading the data from CSV file
boston.data = read.csv("//boston.csv")  #Insert the correct path in quotes

#Subsetting the data into train and test sets (70% Training data; 30% Test data)
subset <- sample(nrow(boston.data), nrow(boston.data) * 0.7)
boston.train = boston.data[subset, ]
boston.test = boston.data[-subset, ]


#Generating different models with different variables to choose a best model
model1 <- lm(medv~., data=boston.train )

nullmodel<-lm(medv~1, data=boston.train)

model2 = step(nullmodel, scope = list(lower = nullmodel, upper = model1), 
                  direction = "forward")

model3<-step(model1, direction = "backward")

model4<- step(nullmodel, scope = list(lower = nullmodel, upper = model1), 
              direction = "both")

#Out of sample performance
pi = predict(model2, boston.test)
mean((pi - boston.test$medv)^2)
mean(abs(pi - boston.test$medv))


#Building Regression Tree
library(rpart)
boston.rpart <- rpart(formula = medv ~ lstat + rm + ptratio + dis + nox + black + chas + zn + 
                        rad + tax + crim, data = boston.train)

boston.rpart <- rpart(formula = medv ~ ., data = boston.train)
par(mar=c(2,4,2,4))
plot(boston.rpart)
text(boston.rpart)

#Performance of regression tree
boston.test.pred.tree = predict(boston.rpart, boston.test)
mean((boston.test.pred.tree - boston.test$medv)^2)


boston.rpart$variable.importance
boston.rpart$cptable
printcp(boston.rpart)
plotcp(boston.rpart)


pruned_tree<-prune(boston.rpart, cp=0.025)
par(mar=c(2,4,2,4))
plot(pruned_tree)
text(pruned_tree)


boston.test.pred.tree = predict(pruned_tree, boston.test)
mean((boston.test.pred.tree - boston.test$medv)^2)

