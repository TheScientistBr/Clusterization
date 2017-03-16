data(iris)

head(iris)
modelCluster <- kmeans(iris[, 1:4], centers = 2)
modelCluster
plot(iris$Sepal.Length, iris$Sepal.Width, pch = 21, 
     bg = c("red", "green")[modelCluster$cluster])

plot(iris[, 1:4], pch = 21, bg = c("red", "green")[modelCluster$cluster])

# No entanto, será que este é o número de cluster mais adequados? 
# Para implementar a análise elbow podemos utilizar a seguinte função:

elbow <- function(dataset) {
        wss <- numeric(15)
        for (i in 1:15) wss[i] <- sum(kmeans(dataset, centers = i, nstart = 100)$withinss)
        plot(1:15, wss, type = "b", main = "Elbow method", xlab = "Number of Clusters", 
             ylab = "Within groups sum of squares", pch = 8)
}

elbow(iris[, 1:4])

clusterModel <- kmeans(iris[, 1:4], centers = 3, nstart = 100)
plot(iris[, 1], iris[, 2], col = clusterModel$cluster, pch = 19)
points(clusterModel$centers, col = 1:3, pch = 19, cex = 2, lwd = 3)

