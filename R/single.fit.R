
#' A single fit function
#'
#'
#' @param training.data a dataframe for training data
#' @param testing.data a dataframe for testing data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., lr, rf, c5.0, and nb (default: "lr")
#' @param classifier.params a list of parameters for an input classifier technique (default: list(rf.ntree = 100, c5.0.trials = 40, c5.0.rules = TRUE)
#' @param params.tuning a boolean indicates whether to perform parameters tuning
#' @import caret C50 e1071 car randomForest
#' @importFrom stats as.formula glm predict
#' @keywords fit
#' @export
fit <-
  function(training.data,
           testing.data,
           dep,
           indep,
           classifier = "lr",
           classifier.params = list(rf.ntree = 100,
                                    c5.0.trials = 40,
                                    c5.0.rules = TRUE),
           params.tuning = F) {
    
    # Generate model formula
    f <-
      as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))
    
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
    
    
    return(list(performance = performance,
                importance = importance,
                model = m))
  }
