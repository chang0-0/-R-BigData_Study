library(party)
library(pROC)
library(mlbench)

data("BreastCancer")
main.ds <- BreastCancer


main.ds <- na.omit(main.ds)
main.ds <- main.ds[complete.cases(main.ds), ]
colSums(is.na(main.ds))


main.ds <- main.ds[!duplicated(main.ds), ]
prop.table(table(main.ds$Class))

Y <- ifelse(
    main.ds$Class == 'malignant', 1, 0
)


X <- main.ds[,c(2:10)]
str(X)

X$Cl.thickness <- as.integer(X$Cl.thickness)
X$Cell.size   <- as.integer(X$Cell.size)
X$Cell.shape <- as.integer(X$Cell.shape)
X$Marg.adhesion <- as.integer(X$Marg.adhesion)
X$Epith.c.size <- as.integer(X$Epith.c.size)
X$Bare.nuclei <- as.integer(X$Bare.nuclei)
X$Bl.cromatin <- as.integer(X$Bl.cromatin)
X$Normal.nucleoli <- as.integer(X$Normal.nucleoli)
X$Mitoses <- as.integer(X$Mitoses)

str(X)
library(PerformanceAnalytics)

windows(width = 8, height = 8)
chart.Correlation(X, histogram = TRUE, col = 'grey10', pch = 1)

X2 <- scale(X)
X2

library(dplyr)
main.ds <- data.frame(Y, X2)
X_names <- names(data.frame(X2))

t.test_p.value_df <- data.frame()
for(i in 1:length(X_names)) {
    t.test_p.value <- t.test(main.ds[, X_names[i]] ~ main.ds$Y,
        var.equal = TRUE
    )$p.value
    t.test_p.value[i, 1] <- X_names[i]
    t.test_p.value[i, 2] <- t.test_p.value
}

head(main.ds)

set.seed(123)
parts <- sample(
    1:nrow(main.ds),
    size = 0.8 * nrow(main.ds)
)


