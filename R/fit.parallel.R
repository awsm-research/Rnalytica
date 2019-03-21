


#' Universal fit function (Parallel)
#'
#'
#' @param data a dataframe for input data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., lr, rf, c5.0, nb, and svm
#' @param classifier.params a list of parameters for an input classifier technique
#' @param params.tuning a boolean indicates whether to perform parameters tuning
#' @param normalize a character for normalization techniques, i.e., log, scale, center, standardize, and no for non-normalization#'
#' @param rebalance a character for a choice of data sampling techniques, i.e., up for upsampling, down for downsampling, and no for no-sampling (default: "NO")
#' @param validation a character for a choice of validation techniques, i.e., boot for bootstrap validation technique, cv for cross-validation technique, and no for constructing a model with the whole dataset without model validation (default: "boot")
#' @param validation.params a list of parameters for an input validation techniques (default: list(cv.k = 10, boot.n = 100))
#' @param prob.threshold a numeric for probability threshold (default: 0.5)
#' @param repeats a numeric for number of repetitions (default: 1)
#' @param n.cores a numeric for number of cores (default: 2)
#' @importFrom caret createFolds downSample upSample
#' @importFrom DMwR SMOTE
#' @importFrom foreach foreach
#' @importFrom stats as.formula glm predict sd
#' @importFrom doParallel registerDoParallel
#' @keywords fit

fit.parallel <-
  function(data,
           dep,
           indep,
           classifier = "lr",
           classifier.params = list(
             rf.ntree = 100,
             # default is the floor(sqrt(#metrics))
             rf.mtry = NULL,
             c5.0.trials = 40,
             c5.0.rules = TRUE,
             c5.0.winnow = FALSE,
             nb.fL = 0,
             nb.adjust = 1,
             # default is 1/#metrics
             svm.gamma = NULL,
             svm.cost = 1
           ),
           params.tuning = FALSE,
           normalize = "no",
           rebalance = "no",
           validation = "boot",
           validation.params = list(cv.k = 10, boot.n = 100),
           prob.threshold = 0.5,
           repeats = 1,
           n.cores = 2) {
    # Init variables
    registerDoParallel(cores = n.cores)
    data.nrow <- nrow(data)
    outcome <- NULL
    if (!is.factor(data[, dep])) {
      outcome <- factor(data[, dep])
      data[, dep] <- factor(data[, dep])
    } else {
      outcome <- data[, dep]
    }
    
    # Generate training and testing samples
    training.indices <- list()
    if (validation == "boot") {
      # Bootstrap sampling (boot)
      for (i in 1:repeats) {
        tmp.indices <- list()
        for (j in 1:validation.params$boot.n) {
          # Generate bootstrap training samples with replacement
          set.seed((i - 1) * 100000 + j)
          tmp.indices[[j]] <- sample(data.nrow, replace = TRUE)
        }
        training.indices[[i]] <- tmp.indices
      }
      seq.length <- validation.params$boot.n
    } else if (validation == "cv") {
      # Cross validation (cv)
      for (i in 1:repeats) {
        # Generate training cv samples
        set.seed(i)
        training.indices[[i]] <-
          createFolds(data[, dep],
                      k = validation.params$cv.k,
                      list = TRUE,
                      returnTrain = TRUE)
      }
      seq.length <- validation.params$cv.k
    } else if (validation != 'no') {
      stop('Invalid input validation technique')
    }
    
    performance <- NULL
    importance <- NULL
    execution.time <- NULL
    # Generate model formula
    f <-
      as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))
    
    if (validation != 'no') {
      # boot and cv
      for (r in 1:repeats) {
        # Repetition loop START
        
        results <- foreach(i = 1:seq.length) %dopar% {
          # for (i in 1:seq.length) {
          # N-Bootstrap or k-cv loop START
          
          # Get training indices at r-th repetition and i-th sample
          indices <- training.indices[[r]][[i]]
          
          # Generate training dataset
          training <- data[indices, ]
          
          # Generate testing dataset
          testing <- data[-unique(indices), ]
          
          ## Data Preprocessing
          
          # Data normalization (default - no)
          train.mean.values <- apply(training[, indep], 2, mean)
          train.sd.values <- apply(training[, indep], 2, sd)
          if (normalize == 'standardize') {
            # normalize training
            tmp <- data.frame(
              sapply(seq_along(indep),
                     function(index, x, x.mean, x.sd) {
                       if (x.sd[index] == 0)
                         return(x[, index])
                       return((x[, index] - x.mean[index]) /
                                x.sd[index])
                     },
                     x = training[, indep],
                     x.mean = train.mean.values,
                     x.sd = train.sd.values)
            )
            tmp <- cbind(tmp, training[, dep])
            names(tmp) <- c(indep, dep)
            training <- tmp
            
            # normalize testing with mean and sd of training (https://stats.stackexchange.com/questions/174823/how-to-apply-standardization-normalization-to-train-and-testset-if-prediction-i)
            tmp <- data.frame(
              sapply(seq_along(indep),
                     function(index, x, x.mean, x.sd) {
                       if (x.sd[index] == 0)
                         return(x[, index])
                       return((x[, index] - x.mean[index]) /
                                x.sd[index])
                     },
                     x = testing[, indep],
                     x.mean = train.mean.values,
                     x.sd = train.sd.values)
            )
            tmp <- cbind(tmp, testing[, dep])
            names(tmp) <- c(indep, dep)
            testing <- tmp
          } else if (normalize == 'scale') {
            # normalize training
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.sd) {
                                       if (x.sd[index] == 0)
                                         return(x[, index])
                                       return((x[, index]) / x.sd[index])
                                     },
                                     x = training[, indep],
                                     x.sd = train.sd.values))
            tmp <- cbind(tmp, training[, dep])
            names(tmp) <- c(indep, dep)
            training <- tmp
            
            # normalize testing with mean and sd of training (https://stats.stackexchange.com/questions/174823/how-to-apply-standardization-normalization-to-train-and-testset-if-prediction-i)
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.sd) {
                                       if (x.sd[index] == 0)
                                         return(x[, index])
                                       return((x[, index]) / x.sd[index])
                                     },
                                     x = testing[, indep],
                                     x.sd = train.sd.values))
            tmp <- cbind(tmp, testing[, dep])
            names(tmp) <- c(indep, dep)
            testing <- tmp
          } else if (normalize == 'center') {
            # normalize training
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.mean) {
                                       return((x[, index] - x.mean[index]))
                                     },
                                     x = training[, indep],
                                     x.mean = train.mean.values))
            tmp <- cbind(tmp, training[, dep])
            names(tmp) <- c(indep, dep)
            training <- tmp
            
            # normalize testing with mean and sd of training (https://stats.stackexchange.com/questions/174823/how-to-apply-standardization-normalization-to-train-and-testset-if-prediction-i)
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.mean) {
                                       return((x[, index] - x.mean[index]))
                                     },
                                     x = testing[, indep],
                                     x.mean = train.mean.values))
            tmp <- cbind(tmp, testing[, dep])
            names(tmp) <- c(indep, dep)
            testing <- tmp
          } else if (normalize == 'log') {
            # normalize training
            training[, indep] <- log1p(training[, indep])
            testing[, indep] <- log1p(testing[, indep])
          } # else no
          
          # Data rebalance (default - no)
          if (rebalance == "down") {
            # Downsampling
            training <-
              downSample(x = training[, indep],
                         y = factor(training[, dep]),
                         yname = dep)
          } else if (rebalance == "up") {
            # Upsampling
            training <-
              upSample(x = training[, indep],
                       y = factor(training[, dep]),
                       yname = dep)
          } else if (rebalance == 'smote') {
            # smote
            training <- SMOTE(f, data = training)
          } # else no
          
          t.start <- Sys.time()
          fit.object <- single.fit(
            training.data = training,
            testing.data = testing,
            dep = dep,
            indep = indep,
            classifier = classifier,
            classifier.params = classifier.params,
            params.tuning = params.tuning,
            prob.threshold = prob.threshold
          )
          t.end <- Sys.time()
          return(
            list(
              performance = fit.object$performance,
              importance = fit.object$importance,
              execution.time = t.end - t.start
            )
          )
        } # n-bootstrap or k-cv loop END
        importance <-
          rbind(importance, do.call(rbind, lapply(results, function(x)
            return(x$importance))))
        performance <-
          rbind(performance, do.call(rbind, lapply(results, function(x)
            return(
              x$performance
            ))))
        execution.time <-
          rbind(execution.time, do.call(rbind, lapply(results, function(x)
            return(
              x$execution.time
            ))))
        
      } # Repetition loop END
    } #else no validation - constructing a model with the whole dataset
    
    # Construct full model
    full.model <- single.fit(
      training.data = data,
      testing.data = NULL,
      dep = dep,
      indep = indep,
      classifier = classifier,
      classifier.params = classifier.params,
      params.tuning = params.tuning,
      prob.threshold = prob.threshold
    )
    
    
    return(
      list(
        performance = performance,
        importance = importance,
        execution.time = execution.time,
        full.model = full.model
      )
    )
  }
