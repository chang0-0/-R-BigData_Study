ds <- esoph
summary(ds)

ds$nsums <- ds$ncases + ds$ncontrols

tail(ds)

nsums_tab <- xtabs(nsums ~ alcgp + tobgp, data = ds)
nsums_tab

chi <- chisq.test(nsums_tab)
result <- chi$statistic
print(result)

