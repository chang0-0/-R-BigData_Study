n <- head(Nile)
print(n)

n.diff1 <- diff(n, differences = 1)
n.diff1

n.diff2 <- diff(n, differences = 2)
n.diff2

Nile.diff1 <- diff(Nile, differences = 1)

# 2차 차분
Nile.diff2 <- diff(Nile, differences = 2)

plot(Nile.diff1)
plot(Nile.diff2)
