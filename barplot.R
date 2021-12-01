library(reshape2)
data_eigenvalue <- melt(apply(as.matrix(data2[, -1])%*% as.matrix(data_princomp$loadings[, ]), 2, var))
