# Kombination von Lernalgorithmen in Ensembles

## Wichtigkeitsmaﬂe

if (!"randomForest" %in% rownames(installed.packages())) {install.packages("randomForest")}
if (!"caret" %in% rownames(installed.packages())) {install.packages("caret")}

library(caret)
library(randomForest)

# Datenvorbereitung
data(airquality, package="datasets")
dataset <- airquality[!(is.na(airquality$Ozone)),]
dataset[is.na(dataset)] <- -1

# Optimierung eines Baums
rf_grid <-  expand.grid(.mtry=c(2,3,5))
rf_model<-train(Ozone ~ ., data=dataset, method="rf",
                trControl=trainControl(method="cv",number=10),
                metric = "RMSE",
                ntree=500,
                importance = TRUE)
print (rf_model)

# Evaluierung der Wichtigkeit von Pr‰diktoren
print (importance(rf_model$finalModel))


