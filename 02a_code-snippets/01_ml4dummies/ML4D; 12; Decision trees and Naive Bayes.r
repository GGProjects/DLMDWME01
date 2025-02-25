# Einfache Lerner

## Klassifikationsbäume und der Greedy-Ansatz

wetter <- expand.grid(Aussicht = c("Sonnig","Bewölkt","Regnerisch"), 
Temperatur = c("Heiß","Mild","Kalt"), Luftfeuchtigkeit=c("Hoch","Normal"), 
Wind=c("Schwach","Stark"))
antwort <- c(1, 19, 4, 31, 16, 2, 11, 23, 35, 6, 24, 15, 18, 36)
spielen <- as.factor(c("Nein", "Nein", "Nein", "Ja", "Ja", "Ja", "Ja", 
"Ja", "Ja", "Ja", "Nein", "Ja", "Ja", "Nein"))
tennis <- data.frame(wetter[antwort,],spielen)

if (!"rpart" %in% rownames(installed.packages())) {install.packages("rpart")}
library(rpart)
tennisbaum <- rpart(spielen ~ ., data=tennis, method="class", 
    parms=list(split="information"), control=rpart.control(minsplit=1))
		
# Ausgabe der Regeln des Entscheidungsbaums
print(tennisbaum)

# Diagrammdarstellung der Baumstruktur
if (!"rpart.plot" %in% rownames(installed.packages())) {install.packages("rpart.plot")}
library(rpart.plot)
prp(tennisbaum, type=0, extra=1, under=TRUE, compress=TRUE)

### Stutzen von großen Bäumen

data(Titanic, package = "datasets")
dataset <- as.data.frame(Titanic)

library(rpart)
titanic_tree <- rpart(Survived ~ Class + Sex + Age, data=dataset, weights=Freq, 
method="class", parms=list(split="information"), control=rpart.control(minsplit=5))

# Ausgabe einer Tabelle mit Überlebensraten
print(aggregate(Freq ~ Survived, data = dataset, sum))

# Zuerst wird die ungestutzte Lösung ausgegeben
library(rpart.plot)
prp(titanic_tree, type=0, extra=1, under=TRUE, compress=TRUE)

# Stutzen des Baums und Darstellung
pruned_titanic_tree <- prune(titanic_tree, cp=0.02)
prp(pruned_titanic_tree, type=0, extra=1, under=TRUE, compress=TRUE)

## Wahrscheinlichkeitsbasierte Algorithmen
### Schätzung mit dem naiven Bayes-Klassifikator

if (!"klaR" %in% rownames(installed.packages())) {install.packages("klaR")}
if (!"kernlab" %in% rownames(installed.packages())) {install.packages("kernlab")}
if (!"e1071" %in% rownames(installed.packages())) {install.packages("e1071")}
library(klaR)
data(spam, package = "kernlab")
print(spam[1,]) # gibt ein Beispiel aus

# Trainingsstichprobe wird erstellt
set.seed(1234)
train_idx <- sample(1:nrow(spam), ceiling(nrow(spam)*3/4), replace=FALSE)

# Naiver Bayes wird angewendet
naive <- NaiveBayes(type ~ ., data=spam[train_idx,], prior = c(0.9,0.1), fL = 0)

# Vorhersage zu E-Mail-Spam wird erstellt
if (!"caret" %in% rownames(installed.packages())) {install.packages("caret")}
library(caret)
predictions <- predict(naive, spam[-train_idx,])
confusionMatrix(predictions$class, spam[-train_idx,"type"])

