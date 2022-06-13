windows(height = 8, width = 8)

data("USArrests")
ds <- USArrests
head(ds)

euc.d <- dist(ds, method = "euclidean")

avg.h <- hclust(euc.d, method = "average")
plot(avg.h)

groups <- cutree(avg.h, k = 6)
groups

plot(avg.h)
rect.hclust(avg.h, k = 6, border = "red")

cmp.h <- hclust(dist(USArrests), method = "complete" )
plot(cmp.h)
rect.hclust(cmp.h, k = 3, border = "red")
rect.hclust(cmp.h, h = 50, which = c(1, 4), border = 3:4 )


# 게층적 군집분석 예제(agnes() 함수)

library(cluster)
cmp.agn <- agnes(
    daisy(USArrests), 
    diss = TRUE,
    metric = "euclidean", 
    method = "complete"
)

par(mfrow = c(1, 2))
plot(cmp.agn )
par(mfrow = c(1, 1))

# install.packages("rattle")
library(rattle)
data("wine")
str(wine)

table(wine$Type)
wine2 <- scale(wine[-1])
wine2

library(foreach)
nc <- 1:15
nc.res <- foreach(i = nc, .combine = rbind) %do% {
    with.ss <- sum(kmeans(wine2, centers = i)$withinss)
    between.ss <- kmeans(wine2, centers = i)$betweenss
    
    return(data.frame(nc = i, wss = with.ss, bss = between.ss))
}

plot(nc.res$nc, nc.res$wss, type = "b", xlab = "Number of Clusters", 
    ylab = "Within groups sum of squares")

plot(nc.res$nc, nc.res$bss, type = "b", xlab = "Number of Clusters", 
    ylab = "Between-cluster sum of squares")

# install.packages("NbClust")
library(NbClust)
set.seed(123)
nbc <- NbClust(wine2, min.nc = 2, max.nc = 15, method = "kmeans")

table(nbc$Best.nc[1,])
barplot(table(nbc$Best.nc[1, ]),
    xlab = "Number of Cluster",
    ylab = "Number of Criteris"
)


set.seed(123)
km.fit <- kmeans(wine2, centers = 3, nstart = 25)
km.fit
