library(dplyr)

data(esoph)
main.ds <- esoph
head(main.ds)

main.ds$nsums <- main.ds$ncases + main.ds$ncontrols
tail(main.ds)

tab <- xtabs(nsums ~ alcgp + tobgp, data = main.ds)

result1 <- chisq.test(tab)
print(result1)
