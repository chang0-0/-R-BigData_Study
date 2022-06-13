install.packages("mlbench")
library(mlbench)
data(Ionosphere)
df = Ionosphere

set.seed(210617)
head(df, 4)

install.packages("rlang", dependencies = TRUE, INSTALL_opts = "--no-lock")
install.packages("scales")

library(rlang)
sessionInfo()

install.packages("caret")
library(caret)

idx = caret::createDataPartition(Ionosphere$Class, p = 0.7)
df_train = Ionosphere[idx$Resample1, ]
df_test = Ionosphere[-idx$Resample1,]

df_train

Sys.getenv('R_LIBS_USER')
fLib <- Sys.getenv('R_LIBS_USER')
fLib

fLibScales <- paste0(fLib, '/rlang')
fLibScales

unlink(fLibScales, recursive=TRUE)
install.packages("rlang")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install('rlang')

install.packages("jsonlite", repos = 'https://cran.r-project.org')

.libPaths()

install.packages("pacman")
install.packages("MASS")
