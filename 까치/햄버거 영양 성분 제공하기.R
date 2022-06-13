burger <- matrix(c(514, 917, 11, 533, 853, 13, 566, 888, 10, 506, 667, 6),
                 nrow=4, byrow=T)

burger

rownames(burger) <- c('M', 'L', 'B','Mo')
colnames(burger) <- c('kcal', 'na', 'fat')
burger

burger['M', 'na']
burger['M',]
burger[,'kcal']
