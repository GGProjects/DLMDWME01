# Komplexere Lernverfahren und neuronale Netze

## Vermeidung von Überanpassung
### Ein Blick hinter die Kulissen: zunächst mit dem Schwertlilien-Datensatz (ein Klassifikationsproblem)

library(MASS)
if (!"neuralnet" %in% rownames(installed.packages())) {install.packages("neuralnet")}
library("neuralnet")

target <- model.matrix( ~ Species - 1, data=iris )
colnames(target) <- c("Setosa", "Versicolor", "Virginica")

set.seed(101)
index <- sample(1:nrow(iris), 100)

train_predictors <- iris[index, 1:4]
test_predictors  <- iris[-index, 1:4]

min_vector   <- apply(train_predictors, 2, min)
range_vector <- apply(train_predictors, 2, max) - apply(train_predictors, 2, min)

train_scaled <- cbind(scale(train_predictors ,min_vector, range_vector), target[index,])
test_scaled  <- cbind(scale(test_predictors ,min_vector, range_vector), target[-index,])

summary(train_scaled)

set.seed(102)
nn_iris <- neuralnet(Setosa + Versicolor + Virginica ~ Sepal.Length + Sepal.Width 
	+ Petal.Length + Petal.Width, data=train_scaled, hidden=c(2), linear.output=F)

plot(nn_iris)

vorhersagen <- compute(nn_iris, test_scaled[,1:4])
y_vorhergesagt <- apply(vorhersagen$net.result,1,which.max)
y_wahr <- apply(test_scaled[,5:7],1,which.max)
confusion_matrix <- table(y_wahr, y_vorhergesagt)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print (confusion_matrix)
print (paste("Genauigkeit:",accuracy))

# Jetzt mit dem Boston-Datensatz (ein Regressionsproblem)

no_examples <- nrow(Boston)
features <- colnames(Boston)

set.seed(101)
index <- sample(1:no_examples, 400)

train <- Boston[index,]
test  <- Boston[-index,]

min_vector <- apply(train,2,min)
range_vector <- apply(train,2,max) - apply(train,2,min)
scaled_train <- scale(train,min_vector,range_vector)
scaled_test  <- scale(test, min_vector,range_vector)

formula = paste("medv ~", paste(features[1:13],collapse='+'))
nn_boston <- neuralnet(formula, data=scaled_train, hidden=c(5,3), linear.output=T)
vorhersagen <- compute(nn_boston, scaled_test[,1:13])
vorhergesagte_werte <- (vorhersagen$net.result * range_vector[14]) + min_vector[14]

RMSE <- sqrt(mean((test[,14] - vorhergesagte_werte)^2))
print (paste("RMSE:",RMSE))
plot(test[,14],vorhergesagte_werte, cex=1.5)
abline(0,1,lwd=1)
