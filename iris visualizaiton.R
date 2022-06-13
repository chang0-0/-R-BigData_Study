library(hrbrthemes)
library(tidyverse)
library(ggridges)
library(ggthemes)
library(cowplot)
library(viridis)
library(GGally)


windows(width = 5, height = 5)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비")
main.ds <- read.csv("iris.csv")

tema = theme(plot.background = element_rect(fill="#F0FFF0"),
                      plot.title = element_text(size=25, hjust=.5),
                      axis.title.x = element_text(size=22, color = "black"),
                      axis.title.y = element_text(size=22, color = "black"),
                      axis.text.x = element_text(size=20),
                      axis.text.y = element_text(size=20),
                      legend.position="bottom",
                      legend.text = element_text(colour="black", size=19, face="bold"))
options(repr.plot.width=14, repr.plot.height=10)

sepallength <- ggplot(data = main.ds, mapping = aes(x = SepalLengthCm)) +
                  geom_density(mapping = aes(fill = Species), color = "black", size = 1.5, alpha = .8) +
                  theme_economist() +
                  xlab("Sepal Length") +
                  ggtitle("Sepal Length by Species") +
                  tema

sepalwidth <- ggplot(data = main.ds, mapping = aes(x = SepalWidthCm)) +
                  geom_density(mapping = aes(fill = Species), color = "black", size = 1.5, alpha = .8) +
                  theme_economist() +
                  xlab("Sepal Width") +
                  ggtitle("Sepal Width by Species") +
                  tema

petallength <- ggplot(data = main.ds, mapping = aes(x = PetalLengthCm)) +
                  geom_density(mapping = aes(fill = Species), color = "black", size = 1.5, alpha = .8) +
                  theme_economist() +
                  xlab("Petal Length") +
                  ggtitle("Petal Length by Species") +
                  tema

petalwidth <- ggplot(data = main.ds, mapping = aes(x = PetalWidthCm)) +
                  geom_density(mapping = aes(fill = Species), color = "black", size = 1.5, alpha = .8) +
                  theme_economist() +
                  xlab("Petal Width") +
                  ggtitle("Petal Width by Species") +
                  tema

plot_grid(sepallength, sepalwidth, petallength, petalwidth, ncol=2, nrow=2)