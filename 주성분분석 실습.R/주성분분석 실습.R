main.ds <- state.x77

head(main.ds)
summary(main.ds)
str(main.ds)

# 8개의 변수
pca <- prcomp(
    main.ds, 
    scale = TRUE
)

pca
summary(pca)

windows(height = 8, width = 8)

plot(
    pca, 
    type = '1',
    pch = 19,
    lwd = 2,
    col = 'red',
    main = "Scree Plot"
)

round(pca$rotation, 3)

round(scale(main.ds) %*% pca$rotation, 3)

round(pca$x, 3)

round(pca$x[, c(1, 2)], 3)

round(cor(pca$x), 3)

# 행렬도

biplot(
    pca,
    cex = c(0,2, 0.4),
    main = 'Biplot'
)
