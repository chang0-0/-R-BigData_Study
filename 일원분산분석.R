ds <- InsectSprays
str(ds)


tapply(ds$count, ds$spray, length)
tapply(ds$count, ds$spray, mean)
tapply(ds$count, ds$spray, sd)

library(gplots)
windows(width = 12, height = 8)
plotmeans(count ~ spray, data = ds )
plotmeans(count ~ spray, data = ds, barcol = "tomato", barwidth = 3, col = "cornflowerblue",
 lwd =2, xlab = "Type of Spray", ylab = "Insect Count", main = "Performance of Insect Sprays")

boxplot(count ~ spray, data = InsectSprays, col = "salmon",
xlab = "Type of Spray", ylab = "Insect Count", main = "Performance of Insect Sprays"
)

#일원 분산 분석

spray.aov <- aov( formula = count ~ spray, data = ds )
spray.aov

summary(spray.aov)
# 대립가설 채택

# 각 집단의 평균
model.tables(spray.aov, type = "mean")
model.tables(spray.aov, type = "effects")

# 사후 분석
# 다중비교

sprays.compare <- TukeyHSD(spray.aov)
sprays.compare

str(sprays.compare)
sprays.compare$spray["D-C",]

plot(sprays.compare)
help(plot)
plot(sprays.compare, col = "blue", las = 1)

install.packages("multcomp")
library(multcomp)
tuk.hsd <- glht(model = spray.aov, linfct = mcp(spray = "Tukey"))

cld(tuk.hsd , level = 0.05)
plot(cld(tuk.hsd, level = 0.05), col = "orange",las = 1)

library(car)
qqplot( InsectSprays$count , id =   )