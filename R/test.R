# 
# 
# 
# library(Rnalytica)
# 
# Data <- loadDefectDataset('eclipse-2.0')
# data <- Data$data
# indep <- Data$indep
# dep <- Data$dep
# 
# fit(
#   data,
#   dep,
#   indep,
#   classifier = 'svm',
#   validation.params = list(boot.n = 2)
# )
# 
# 
# fit.parallel(
#   data,
#   dep,
#   indep,
#   classifier = 'svm',
#   validation.params = list(boot.n = 4),
#   nCore = 4
# )
