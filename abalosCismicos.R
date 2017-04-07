library("cluster")
library("ClustOfVar")
library("nnet")

eData <- read.csv("data/earthquakeData.csv")

names(eData)
dim(eData)
sapply(eData, class)

summary(eData$mag)

summary(eData$depth)

tree <- hclustvar(eData[,c(2:5)],eData[,c(6,22)])

# Um procedimento bootstrap também é proposto para avaliar a estabilidade das partições.
# Em K = 2, 3,. . . , P - 1 clusters e, em seguida, para ajudar o usuário a determinar 
# um número adequado de grupos de variáveis.

stab <- stability(tree, graph = FALSE,B = 100)

nrCluster <- which.is.max(stab$meanCR)
nrCluster
plot(stab)

set.seed(1234)
clusterModel <- kmeans(iris[, 1:4], centers = 3, nstart = 100)
clusterModel

plot(eData$mag, eData$depth, col = clusterModel$cluster, pch = 19, xlab = "Magnitude", 
     ylab = "Depth", main = "Terremotos")

eData[clusterModel$cluster == 1, ]


pca <- princomp(eData[,c(2:5)], cor=T)
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]
newComp <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(newComp, nrCluster)
plot(pc.comp1, pc.comp2,col=clusterModel$cluster,xlab = "Componente 1", ylab = "Componente 2",
     main = "KMeans & PCA")
points(cl$centers, pch=19)

