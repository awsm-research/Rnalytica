

#' Universal fit function
#'
#'
#' @param data a dataframe for input data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., lr, rf, c5.0, and nb (default: "lr")
#' @param classifier.params a list of parameters for an input classifier technique (default: list(rf.ntree = 100, c5.0.trials = 40, c5.0.rules = TRUE)
#' @param rebalance a character for a choice of data sampling techniques, i.e., up for upsampling, down for downsampling, and no for no-sampling (default: "NO")
#' @param validation a character for a choice of validation techniques, i.e., boot for bootstrap validation technique, cv for cross-validation technique, and no for constructing a model with the whole dataset without model validation (default: "boot")
#' @param validation.params a list of parameters for an input validation techniques (default: list(cv.k = 10, boot.n = 100))
#' @param prob.threshold a numeric for probability threshold (default: 0.5)
#' @param repeats a numeric for number of repetitions (default: 1)
#' @import caret C50 e1071
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
           rebalance = "no",
           validation = "boot",
           validation.params = list(cv.k = 10, boot.n = 100),
           prob.threshold = 0.5,
           repeats = 1) {
    # Init variables
    results <- list()
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

          # Generate model formula
          f <-
            as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))

          # Data Preprocessing (default - no)
          if (rebalance == "down") {
            # Downsampling
            training <-
              downSample(x = training[, indep],
                         y = training[, dep],
                         yname = dep)
          } else if (rebalance == "up") {
            # Upsampling
            training <-
              upSample(x = training[, indep],
                       y = training[, dep],
                       yname = dep)
          } # else no

          if (classifier == "lr") {
            m <- glm(f, data = training, family = "binomial")
            importance <-
              rbind(importance, c(repetition = r, Anova(m)$"LR Chisq"))
            prob <- predict(m, testing, type = "response")
          } else if (classifier == "rf") {
            m <- randomForest(x = training[, indep],
                              y = outcome[indices],
                              ntree = classifier.params$rf.ntree)
            prob <-
              predict(m, newdata = testing[, indep], type = 'prob')[, "TRUE"]
            importance <-
              rbind(importance, c(repetition = r, varImp(m)$Overall))
          } else if (classifier == "c5.0") {
            m <- C5.0(
              training[, indep],
              outcome[indices],
              trials = classifier.params$c5.0.trials,
              rules = classifier.params$c5.0.rules
            )
            prob <-
              predict(m, newdata = testing, type = "prob")[, "TRUE"]
            importance <-
              rbind(importance, c(repetition = r, varImp(m)$Overall))
          } else if (classifier == "nb") {
            m <- naiveBayes(f, data = training)
            prob <-
              predict(m, newdata = testing, type = "raw")[, "TRUE"]

            # Permutation Importance for Naive Bayes
            genericVarImp <- c()
            for (predictorName in indep) {
              shuffledData <- testing[, indep]
              shuffledData[, predictorName] <-
                sample(shuffledData[, predictorName], length(shuffledData[, predictorName]))
              predictions <-
                predict(m, newdata = shuffledData, type = "raw")[, "TRUE"]
              predictions <- ifelse(predictions > 0.5, TRUE, FALSE)
              genericVarImp <-
                cbind(genericVarImp, mean(testing[, dep] != predictions))
            }
            colnames(genericVarImp) <- indep
            importance <-
              rbind(importance, c(repetition = r, genericVarImp))
          }

          # Compute performance
          if (classifier %in% c("rf", "c5.0")) {
            # The dependent variable must be factor for rf and c5.0
            performance <-
              rbind(performance,
                    c(repetition = r, performance.calculation(outcome[-unique(indices)], prob, prob.threshold)))
          } else {
            performance <-
              rbind(performance,
                    c(repetition =r, performance.calculation(testing[, dep], prob, prob.threshold)))
          }

        } # n-bootstrap or k-cv loop END
      } # Repetition loop END
      performance <- data.frame(performance)
      importance <- data.frame(importance)
      names(importance) <- c('repetition', indep)
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
