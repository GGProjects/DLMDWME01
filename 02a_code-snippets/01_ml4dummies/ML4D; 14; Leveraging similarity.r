# Ausnutzung von Ähnlichkeiten in Daten

## Experimente zur Konvergenz von Zentroiden

# Aufruf der Bibliotheken
library(datasets)
library(class)

# wir zerlegen unseren Datensatz in Antwort und Merkmale
answer <- iris[,5]
features <- iris[,1:4]
X <- princomp(features)$scores

clustering <- kmeans(x=X, centers=3, iter.max = 999, nstart = 10,
       algorithm = "Hartigan-Wong")

print (clustering$tot.withinss)
table(answer, clustering$cluster)

w <- rep(0,10)
for (h in 1:10) {
  clustering <- kmeans(x=X, centers=h, iter.max = 999, nstart = 10,
                       algorithm = "Hartigan-Wong")
  w[h] <- clustering$tot.withinss
}

plot(w, type='o')

clustering <- kmeans(x=X, centers=8, iter.max = 999, nstart = 10,
                     algorithm = "Hartigan-Wong")

table(answer, clustering$cluster)

plot(X[,c(1,2)], col = clustering$cluster)
points(clustering$centers[,c(1,2)], col = 1:8, pch = 15, cex = 1.2)

## Experimente mit einem flexiblen Algorithmus

# mit festem Seed-Wert zur Reproduzierbarkeit wird Teststichprobe erstellt
set.seed(seed=101)
out_of_sample <- sample(x=length(answer),25)

# in einer Schleife werden Werte von 1 bis 15 für k getestet
for (h in 1:15) {
  
  in_sample_pred <- knn.cv(train=features[-out_of_sample,], cl=answer[-out_of_sample], 
                         k = h, l = 0, prob = FALSE, use.all = TRUE)
  # nach Erhalt der kreuzvalidierten Vorhersagen berechnen wir die Genauigkeit
  accuracy <- sum(answer[-out_of_sample]==in_sample_pred) / length(answer[-out_of_sample])
  # das Ergebnis wird ausgegeben
  print (paste("für k=",h," ist die Genauigkeit:",accuracy))
}

out_sample_pred <- knn(train=features[-out_of_sample,], test=features[out_of_sample,], 
                 cl=answer[-out_of_sample], k = 11, l = 0, prob = TRUE, use.all = TRUE)

print (table(answer[out_of_sample], out_sample_pred))
