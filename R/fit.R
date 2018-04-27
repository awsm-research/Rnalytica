




#' Universal fit function
#'
#'
#' @param data a dataframe for input data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., LR, RF, C5.0, and NB (default: "LR")
#' @param classifier.params a list of parameters for an input classifier technique (default: list(RF.ntree = 100, C5.0.trials = 40, C5.0.rules = TRUE)
#' @param rebalance a character for a choice of data sampling techniques, i.e., UP for upsampling, DOWN for downsampling, and NO for no-sampling (default: "NO")
#' @param validation a character for a choice of validation techniques, i.e., BOOT for bootstrap validation technique, and CV for cross-validation technique (default: "BOOT")
#' @param validation.params a list of parameters for an input validation techniques (default: list(CV.k = 10, BOOT.N = 100))
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
           classifier = "LR",
           classifier.params = list(RF.ntree = 100,
                                    C5.0.trials = 40,
                                    C5.0.rules = TRUE),
           rebalance = "NO",
           validation = "BOOT",
           validation.params = list(CV.k = 10, BOOT.N = 100),
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
    if (validation == "BOOT") {
      # Bootstrap sampling (BOOT)
      for (i in 1:repeats) {
        tmp.indices <- list()
        for (j in 1:validation.params$BOOT.N) {
          # Generate bootstrap training samples with replacement
          set.seed((i - 1) * 100000 + j)
          tmp.indices[[j]] <- sample(data.nrow, replace = TRUE)
        }
        training.indices[[i]] <- tmp.indices
      }
      seq.length <- validation.params$BOOT.N
    } else if (validation == "CV") {
      # Cross validation (CV)
      for (i in 1:repeats) {
        # Generate training CV samples
        set.seed(i)
        training.indices[[i]] <-
          createFolds(data[, dep],
                      k = validation.params$CV.k,
                      list = TRUE,
                      returnTrain = TRUE)
      }
      seq.length <- validation.params$CV.k
    } else {
      stop('Invalid input validation technique')
    }

    for (r in 1:repeats) {
      # Repetition loop START

      perf.results <- NULL
      important.scores <- NULL
      for (i in 1:seq.length) {
        # N-Bootstrap or k-CV loop START

        # Get training indices at r-th repetition and i-th sample
        indices <- training.indices[[r]][[i]]

        # Generate training dataset
        training <- data[indices,]

        # Generate testing dataset
        testing <- data[-unique(indices),]

        # Generate model formula
        f <-
          as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))

        # Data Preprocessing (default - NO)
        if (rebalance == "DOWN") {
          # Downsampling
          training <-
            downSample(x = training[, indep],
                       y = training[, dep],
                       yname = dep)
        } else if (rebalance == "UP") {
          # Upsampling
          training <-
            upSample(x = training[, indep],
                     y = training[, dep],
                     yname = dep)
        } # else NO

        if (classifier == "LR") {
          m <- glm(f, data = training, family = "binomial")
          important.scores <-
            rbind(important.scores, Anova(m)$"LR Chisq")
          prob <- predict(m, testing, type = "response")
        } else if (classifier == "RF") {
          m <- randomForest(x = training[, indep],
                            y = outcome[indices],
                            ntree = classifier.params$RF.ntree)
          prob <- predict(m, newdata = testing[, indep], type = 'prob')[, "TRUE"]
          important.scores <-
            rbind(important.scores, varImp(m)$Overall)
        } else if (classifier == "C5.0") {
          m <- C5.0(
            training[, indep],
            outcome[indices],
            trials = classifier.params$C5.0.trials,
            rules = classifier.params$C5.0.rules
          )
          prob <-
            predict(m, newdata = testing, type = "prob")[, "TRUE"]
          important.scores <-
            rbind(important.scores, varImp(m)$Overall)
        } else if (classifier == "NB") {
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
          important.scores <-
            rbind(important.scores, genericVarImp)
        }

        # Compute performance
        if(classifier %in% c("RF", "C5.0")){ # The dependent variable must be factor for RF and C5.0
          perf.results <-
            rbind(perf.results,
                  performance.calculation(outcome[-unique(indices)], prob, prob.threshold))
        } else {
          perf.results <-
            rbind(perf.results,
                  performance.calculation(testing[, dep], prob, prob.threshold))
        }

      } # N-Bootstrap or k-CV loop END

      important.scores <- data.frame(important.scores)
      colnames(important.scores) <- indep
      results[[r]] <-
        list(performance.estimates = data.frame(perf.results),
             important.scores = important.scores)

    } # Repetition loop END

    # Construct full model
    if (classifier == "LR") {
      full.model <- glm(f, data = data, family = "binomial")
    } else if (classifier == "RF") {
      full.model <- randomForest(x = data[, indep],
                                 y = outcome,
                                 ntree = classifier.params$RF.ntree)
    } else if (classifier == "C5.0") {
      full.model <- C5.0(data[, indep],
                         outcome,
                         trials = classifier.params$C5.0.trials,
                         rules = classifier.params$C5.0.rules)
    } else if (classifier == "NB") {
      full.model <- naiveBayes(f, data = data)
    }

    names(results) <- paste0('Repetition-', 1:repeats)
    return(list(results = results,
                full.model = full.model))
  }

