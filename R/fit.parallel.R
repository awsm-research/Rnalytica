
#' Universal fit function (Parallel implementation)
#'
#'
#' @param data a dataframe for input data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., lr, rf, c5.0, and nb (default: "lr")
#' @param classifier.params a list of parameters for an input classifier technique (default: list(rf.ntree = 100, c5.0.trials = 40, c5.0.rules = TRUE)
#' @param params.tuning a boolean indicates whether to perform parameters tuning
#' @param normalize a character for normalization techniques, i.e., log, scale, center, standardize, and no for non-normalization#' 
#' @param rebalance a character for a choice of data sampling techniques, i.e., up for upsampling, down for downsampling, and no for no-sampling (default: "NO")
#' @param validation a character for a choice of validation techniques, i.e., boot for bootstrap validation technique, cv for cross-validation technique, and no for constructing a model with the whole dataset without model validation (default: "boot")
#' @param validation.params a list of parameters for an input validation techniques (default: list(cv.k = 10, boot.n = 100))
#' @param prob.threshold a numeric for probability threshold (default: 0.5)
#' @param repeats a numeric for number of repetitions (default: 1)
#' @import caret C50 e1071 car randomForest DMwR doMC
#' @importFrom stats as.formula glm predict
#' @keywords fit
#' @export
fit.parallel <-
  function(data,
           dep,
           indep,
           classifier = "lr",
           classifier.params = list(rf.ntree = 100,
                                    c5.0.trials = 40,
                                    c5.0.rules = TRUE),
           params.tuning = FALSE,
           normalize = "no",
           rebalance = "no",
           validation = "boot",
           validation.params = list(cv.k = 10, boot.n = 100),
           prob.threshold = 0.5,
           repeats = 1,
           nCore = 2) {
    # Init variables
    registerDoMC(nCore)
    data.nrow <- nrow(data)
    outcome <- NULL
    if (!is.factor(data[, dep])) {
      outcome <- factor(data[, dep])
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
          training <- data[indices,]
          
          # Generate testing dataset
          testing <- data[-unique(indices),]
          
          ## Data Preprocessing 
          
          # Data normalization (default - no)
          train.mean.values <- apply(training[, indep], 2, mean)
          train.sd.values <- apply(training[, indep], 2, sd)
          if(normalize == 'standardize'){
            # normalize training
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.mean, x.sd){
                                       return((x[, index] - x.mean[index])/x.sd[index])
                                     },
                                     x = training[, indep],
                                     x.mean = train.mean.values,
                                     x.sd = train.sd.values))
            tmp <- cbind(tmp, training[, dep])
            names(tmp) <- c(indep, dep)
            training <- tmp
            
            # normalize testing with mean and sd of training (https://stats.stackexchange.com/questions/174823/how-to-apply-standardization-normalization-to-train-and-testset-if-prediction-i)
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.mean, x.sd){
                                       return((x[, index] - x.mean[index])/x.sd[index])
                                     },
                                     x = testing[, indep],
                                     x.mean = train.mean.values,
                                     x.sd = train.sd.values))
            tmp <- cbind(tmp, testing[, dep])
            names(tmp) <- c(indep, dep)
            testing <- tmp
          } else if(normalize == 'scale'){
            # normalize training
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.sd){
                                       return((x[, index])/x.sd[index])
                                     },
                                     x = training[, indep],
                                     x.sd = train.sd.values))
            tmp <- cbind(tmp, training[, dep])
            names(tmp) <- c(indep, dep)
            training <- tmp
            
            # normalize testing with mean and sd of training (https://stats.stackexchange.com/questions/174823/how-to-apply-standardization-normalization-to-train-and-testset-if-prediction-i)
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.sd){
                                       return((x[, index])/x.sd[index])
                                     },
                                     x = testing[, indep],
                                     x.sd = train.sd.values))
            tmp <- cbind(tmp, testing[, dep])
            names(tmp) <- c(indep, dep)
            testing <- tmp
          } else if(normalize == 'center'){
            # normalize training
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.mean){
                                       return((x[, index] - x.mean[index]))
                                     },
                                     x = training[, indep],
                                     x.mean = train.mean.values))
            tmp <- cbind(tmp, training[, dep])
            names(tmp) <- c(indep, dep)
            training <- tmp
            
            # normalize testing with mean and sd of training (https://stats.stackexchange.com/questions/174823/how-to-apply-standardization-normalization-to-train-and-testset-if-prediction-i)
            tmp <- data.frame(sapply(seq_along(indep),
                                     function(index, x, x.mean){
                                       return((x[, index] - x.mean[index]))
                                     },
                                     x = testing[, indep],
                                     x.mean = train.mean.values))
            tmp <- cbind(tmp, testing[, dep])
            names(tmp) <- c(indep, dep)
            testing <- tmp
          } else if(normalize == 'log'){
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
          
          # TODO WORK ON PARAMETER TUNING
          # if(params.tuning){ 
          #   if (classifier == "lr") {
          #     
          #   } else if (classifier == "rf") {
          #     
          #   } else if (classifier == "c5.0") {
          #     
          #   } else if (classifier == "nb") {
          #   }  else if (classifier == "svm") {
          #     tune.svm(f, data = training, kernel="radial", cost=10^(-1:2), gamma=c(.5,1,2))
          #   }
          # }
          
          t.start <- Sys.time()
          fit.object <- single.fit(training,
                                   testing,
                                   dep,
                                   indep,
                                   classifier,
                                   classifier.params,
                                   params.tuning)
          t.end <- Sys.time()
          return(list(performance = fit.object$performance, importance = fit.object$importance, execution.time = t.end - t.start))
        } # n-bootstrap or k-cv loop END
        importance <- rbind(importance, do.call(rbind, lapply(results, function(x) return(x$importance))))
        performance <- rbind(performance, do.call(rbind, lapply(results, function(x) return(x$performance))))
        execution.time <- rbind(execution.time, do.call(rbind, lapply(results, function(x) return(x$execution.time))))
        
      } # Repetition loop END
    } #else no validation - constructing a model with the whole dataset
    
    # Construct full model
    if (classifier == "lr") {
      full.model <- glm(f, data = data, family = "binomial")
    } else if (classifier == "rf") {
      full.model <- randomForest(x = data[, indep],
                                 y = outcome,
                                 ntree = classifier.params$rf.ntree)
    } else if (classifier == "c5.0") {
      full.model <- C5.0(data[, indep],
                         outcome,
                         trials = classifier.params$c5.0.trials,
                         rules = classifier.params$c5.0.rules)
    } else if (classifier == "nb") {
      full.model <- naiveBayes(f, data = data)
    }
    
    return(list(performance = performance,
                importance = importance,
                execution.time = execution.time,
                full.model = full.model))
  }
