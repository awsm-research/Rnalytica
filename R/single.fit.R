

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
single.fit <-
  function(training.data,
           testing.data,
           dep,
           indep,
           classifier = "lr",
           classifier.params = list(rf.ntree = 100,
                                    c5.0.trials = 40,
                                    c5.0.rules = TRUE),
           params.tuning = F) {
    importance <- NULL
    performance <- NULL
    training.data[, dep] <- factor(training.data[, dep])
    testing.data[, dep] <- factor(testing.data[, dep])
    # Generate model formula
    f <-
      as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))
    
    if (classifier == "lr") {
      m <- glm(f, data = training.data, family = "binomial")
      importance <-
        rbind(importance, c(repetition = r, Anova(m)$"LR Chisq"))
      prob <- predict(m, testing.data, type = "response")
    } else if (classifier == "rf") {
      # Traditional randomForest
      # set.seed(1)
      # m <- randomForest(x = training.data[, indep],
      #                   y = training.data[, dep],
      #                   ntree = classifier.params$rf.ntree)
      # prob <-
      #   predict(m, newdata = testing.data[, indep], type = 'prob')[, "TRUE"]
      # importance <-
      #   rbind(importance, c(repetition = r, varImp(m)$Overall))
      
      # Faster randomForest (ranger)
      set.seed(1)
      m <-
        ranger(
          f,
          data = training.data,
          num.trees = classifier.params$rf.ntree,
          classification = TRUE,
          importance = "permutation"
        )
      prob <-
        predict(m,
                data = testing.data,
                type = 'response',
                predict.all = T)
      prob <-
        apply(prob$predictions, 1, function(x) {
          o <-
            table(x)
          o <- o / sum(o)
          return(ifelse(is.na(o['2']), 0, o['2']))
        })
      names(prob) <- row.names(testing.data)
      importance <-
        rbind(importance, c(repetition = r, importance(m)))
      
    } else if (classifier == "c5.0") {
      m <- C5.0(
        training.data[, indep],
        training.data[, dep],
        trials = classifier.params$c5.0.trials,
        rules = classifier.params$c5.0.rules
      )
      prob <-
        predict(m, newdata = testing.data, type = "prob")[, "TRUE"]
      importance <-
        rbind(importance, c(repetition = r, varImp(m)$Overall))
    } else if (classifier == "nb") {
      m <- naiveBayes(f, data = training.data)
      prob <-
        predict(m, newdata = testing.data, type = "raw")[, "TRUE"]
      
      # Permutation Importance for Naive Bayes
      genericVarImp <- c()
      for (predictorName in indep) {
        shuffledData <- testing.data[, indep]
        shuffledData[, predictorName] <-
          sample(shuffledData[, predictorName], length(shuffledData[, predictorName]))
        predictions <-
          predict(m, newdata = shuffledData, type = "raw")[, "TRUE"]
        predictions <- ifelse(predictions > 0.5, TRUE, FALSE)
        genericVarImp <-
          cbind(genericVarImp, mean(testing.data[, dep] != predictions))
      }
      colnames(genericVarImp) <- indep
      importance <-
        rbind(importance, c(repetition = r, genericVarImp))
    }  else if (classifier == "svm") {
      m <- svm(f, data=training.data, probability = TRUE)
      prob <- predict(m, testing.data, probability = TRUE)
      prob <- attr(prob, "probabilities")[, 'TRUE']
      names(prob) <- row.names(testing.data)
      # TODO
      importance <-
        rbind(importance, c(repetition = r, rep(0, length(indep))))
    }
    
    # Compute performance
    if (classifier %in% c("rf", "c5.0")) {
      # The dependent variable must be factor for rf and c5.0
      # outcome <- NULL
      # if (!is.factor(testing.data[, dep])) {
      #   outcome <- factor(testing.data[, dep])
      # } else {
      #   outcome <- testing.data[, dep]
      # }
      performance <-
        rbind(performance,
              c(
                repetition = r,
                performance.calculation(factor(testing.data[, dep]), prob, prob.threshold)
              ))
    } else {
      performance <-
        rbind(performance,
              c(
                repetition = r,
                performance.calculation(testing.data[, dep], prob, prob.threshold)
              ))
    }
    
    importance <- data.frame(importance)
    names(importance) <- c('repetition', indep)
    return(list(
      performance = data.frame(performance),
      importance = importance,
      model = m
    ))
  }
