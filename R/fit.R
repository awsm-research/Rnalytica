
#' Universal fit function
#'
#'
#' @param data a dataframe for input data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., lr, rf, c5.0, and nb (default: "lr")
#' @param classifier.params a list of parameters for an input classifier technique (default: list(rf.ntree = 100, c5.0.trials = 40, c5.0.rules = TRUE)
#' @param params.tuning a boolean indicates whether to perform parameters tuning
#' @param rebalance a character for a choice of data sampling techniques, i.e., up for upsampling, down for downsampling, and no for no-sampling (default: "NO")
#' @param validation a character for a choice of validation techniques, i.e., boot for bootstrap validation technique, cv for cross-validation technique, and no for constructing a model with the whole dataset without model validation (default: "boot")
#' @param validation.params a list of parameters for an input validation techniques (default: list(cv.k = 10, boot.n = 100))
#' @param prob.threshold a numeric for probability threshold (default: 0.5)
#' @param repeats a numeric for number of repetitions (default: 1)
#' @import caret C50 e1071 car randomForest
#' @importFrom stats as.formula glm predict
#' @keywords fit
#' @export
fit <-
  function(data,
           dep,
           indep,
           classifier = "lr",
           classifier.params = list(rf.ntree = 100,
                                    c5.0.trials = 40,
                                    c5.0.rules = TRUE),
           params.tuning = FALSE,
           rebalance = "no",
           validation = "boot",
           validation.params = list(cv.k = 10, boot.n = 100),
           prob.threshold = 0.5,
           repeats = 1) {
    # Init variables
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
    # Generate model formula
    f <-
      as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))
    
    if (validation != 'no') {
      # boot and cv
      for (r in 1:repeats) {
        # Repetition loop START
        
        for (i in 1:seq.length) {
          # N-Bootstrap or k-cv loop START
          
          # Get training indices at r-th repetition and i-th sample
          indices <- training.indices[[r]][[i]]
          
          # Generate training dataset
          training <- data[indices,]
          
          # Generate testing dataset
          testing <- data[-unique(indices),]
          
          # Data Preprocessing (default - no)
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
          } # else no
          
          fit.object <- single.fit(training,
                                   testing,
                                   dep,
                                   indep,
                                   classifier,
                                   classifier.params,
                                   params.tuning)
          
          importance <- rbind(importance, fit.object$importance)
          performance <- rbind(performance, fit.object$performance)
          
        } # n-bootstrap or k-cv loop END
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
                full.model = full.model))
  }
