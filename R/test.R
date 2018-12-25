# #
# #
# # #
# library(Rnalytica)

# Data <- loadDefectDataset('eclipse-2.0')
# data <- Data$data
# indep <- Data$indep
# dep <- Data$dep

# fit(
#   data,
#   dep,
#   indep,
#   classifier = 'svm',
#   validation.params = list(boot.n = 2)
# )


# fit.parallel(
#   data,
#   dep,
#   indep,
#   classifier = 'svm',
#   validation.params = list(boot.n = 4),
#   nCore = 4
# )
# #
# dataset.paths <- list.files('/Users/jirayusjiarpakdee/Dropbox/Source/Repository/reevaluate-factors-performance/datasets/')
#
# i = 1
# # for(i in seq_along(dataset.paths)){
#
# # Load and process dataset
# library(Jmisc)
# dataset <- read.csv(paste0('/Users/jirayusjiarpakdee/Dropbox/Source/Repository/reevaluate-factors-performance/datasets/', dataset.paths[i]))
# indep <- names(dataset)[2:(length(dataset) - 6)]
# dep <- 'Type_3'
# dataset <- dataset[, c(indep, dep)]
# names(dataset)[length(dataset)] <- 'defect'
# dep <- 'defect'
# dataset[, dep] <- change.factor.levels(dataset$defect, levels(dataset$defect), c(TRUE, FALSE))
# set.seed(1)
# indices <- sample(nrow(dataset), replace = TRUE)
# training.data <- dataset[indices,]
# testing.data <- dataset[-indices,]

# data = dataset
# classifier = "svm"
# classifier.params = list(rf.ntree = 100,
#                          c5.0.trials = 40,
#                          c5.0.rules = TRUE)
# params.tuning = FALSE
# normalize = "standardize"
# rebalance = "no"
# validation = "boot"
# validation.params = list(cv.k = 10, boot.n = 4)
# prob.threshold = 0.5
# repeats = 1
# nCore = 4

# Toy dataset testing all functions
# dataset <- dataset[1:100,]
# classifiers <- c('lr', 'rf', 'svm', 'c5.0', 'nb')
# for(c.index in classifiers){
#   fit.parallel(dataset, dep, indep, classifier = c.index, validation.params = list(boot.n=4), params.tuning = T)
#   print(paste0(c.index, ' done'))
# }


# fit.parallel(dataset,
#          dep,
#          indep,
#          classifier = "svm",
#          classifier.params = list(rf.ntree = 100,
#                                   c5.0.trials = 40,
#                                   c5.0.rules = TRUE),
#          params.tuning = FALSE,
#          normalize = "standardize",
#          rebalance = "no",
#          validation = "boot",
#          validation.params = list(cv.k = 10, boot.n = 4),
#          prob.threshold = 0.5,
#          repeats = 1,
#          nCore = 4)
#
