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
stab <- stability(tree, graph = FALSE,B = 10)

nrCluster <- which.is.max(stab$meanCR)
nrCluster

set.seed(1234)

plot(eData$mag, eData$depth, col = clusterModel$cluster, pch = 19, xlab = "Magnitude", 
     ylab = "Depth", main = "Terremotos")
par(new=TRUE)

clusterModel

eData[clusterModel$cluster == 1, ]


pca <- princomp(eData[,c(2:5)], cor=T)
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]
newComp <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(newComp, nrCluster)
plot(pc.comp1, pc.comp2,col=cl$cluster,xlab = "Componente 1", ylab = "Componente 2",
     main = "KMeans & PCA")
points(cl$centers, pch=16)
