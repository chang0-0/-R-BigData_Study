iris.sub <- subset(iris, select = c("Sepal.Length", "Sepal.Width", "Species"), 
subset = Species %in% c("setosa", "virginica"))

iris.sub$Species <- factor(iris.sub$Species)
head(iris.sub)
tail(iris.sub)

library(ggplot2)
ggplot(iris.sub, aes(x = Sepal.Length, y = Sepal.Width)) +
 geom_point(aes(color = Species, shape = Species), size = 2)

library(e1071)
set.seed(123)
iris.svm <- svm(Species ~ . , data = iris.sub, kernel = "linear", cost = 1, scale = FALSE)
summary(iris.svm)



  
