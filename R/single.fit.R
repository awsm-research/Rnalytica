
#' A single fit function
#'
#'
#' @param training.data a dataframe for training data
#' @param testing.data a dataframe for testing data
#' @param dep a character for dependent variable
#' @param indep a vector of characters for independent variables
#' @param classifier a character for classifier techniques, i.e., lr, rf, c5.0, nb, and svm
#' @param classifier.params a list of parameters for an input classifier technique
#' @param params.tuning a boolean indicates whether to perform parameters tuning
#' @param prob.threshold a numeric for probability threshold (default: 0.5)
#' @importFrom caret train
#' @importFrom C50 C5.0 
#' @importFrom ranger ranger
#' @importFrom e1071 svm
#' @importFrom stats as.formula glm predict
#' @keywords fit
#' @export
single.fit <-
  function(training.data,
           testing.data,
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
           params.tuning = F,
           prob.threshold = 0.5) {
    no.testing <- !is.null(testing.data)
    
    
    importance <- NULL
    performance <- NULL
    training.data[, dep] <- factor(training.data[, dep])
    if (no.testing)
      testing.data[, dep] <- factor(testing.data[, dep])
    # Generate model formula
    f <-
      as.formula(paste0(dep, " ~ ", paste0(indep, collapse = "+")))
    
    if (classifier == "lr") {
      # Unfortunately, glm cannot be tuned
      m <- glm(f, data = training.data, family = "binomial")
      importance <-
        rbind(importance, c(Anova(m)$"LR Chisq"))
      if (no.testing)
        prob <- predict(m, testing.data, type = "response")
    } else if (classifier == "rf") {
      if (params.tuning) {
        # Tuning mtry - Random Search (https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/)
        # 10 random and 10 bootstrap validation
        tune.data <- training.data
        tune.data[, dep] <-
          factor(tune.data[, dep], labels = make.names(levels(tune.data[, dep])))
        control <-
          trainControl(
            method = "boot",
            number = 10,
            search = "random",
            summaryFunction = twoClassSummary,
            classProbs = T
          )
        set.seed(1)
        rf_random <-
          train(
            f,
            data = tune.data,
            method = "rf",
            metric = "ROC",
            tuneLength = 10,
            trControl = control
          )
        # Use the best mtry
        classifier.params$rf.mtry <- rf_random$bestTune$mtry
      }
      # Faster randomForest (ranger)
      set.seed(1)
      m <-
        ranger(
          f,
          data = training.data,
          num.trees = classifier.params$rf.ntree,
          mtry = classifier.params$rf.mtry,
          classification = TRUE,
          importance = "permutation"
        )
      
      if (no.testing) {
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
      }
      
      importance <-
        rbind(importance, c(ranger::importance(m)))
      
    } else if (classifier == "c5.0") {
      if (params.tuning) {
        # Tuning trials, rules, and winnow - Random Search (https://www.euclidean.com/machine-learning-in-practice/2015/6/12/r-caret-and-parameter-tuning-c50)
        # 10 random and 10 bootstrap validation
        tune.data <- training.data
        tune.data[, dep] <-
          factor(tune.data[, dep], labels = make.names(levels(tune.data[, dep])))
        control <-
          trainControl(
            method = "boot",
            number = 10,
            search = "random",
            summaryFunction = twoClassSummary,
            classProbs = T
          )
        set.seed(1)
        c50_random <-
          train(
            f,
            data = tune.data,
            method = "C5.0",
            metric = "ROC",
            tuneLength = 10,
            trControl = control
          )
        # Use the best trials, model type, and winnow
        classifier.params$c5.0.trials <- c50_random$bestTune$trials
        classifier.params$c5.0.rules <-
          c50_random$bestTune$model == 'rules'
        classifier.params$c5.0.winnow <- c50_random$bestTune$winnow
      }
      
      m <- C5.0(
        training.data[, indep],
        training.data[, dep],
        trials = classifier.params$c5.0.trials,
        rules = classifier.params$c5.0.rules,
        control = C5.0Control(winnow = classifier.params$c5.0.winnow)
      )
      importance <-
        rbind(importance, c(varImp(m)$Overall))
      
      if (no.testing) {
        prob <-
          predict(m, newdata = testing.data, type = "prob")[, "TRUE"]
      }
      
    } else if (classifier == "nb") {
      if (params.tuning) {
        # Tuning fL and adjust - Grid Search
        # 9 combinations of fL and adjust x 10 bootstrap validation
        tune.data <- training.data
        tune.data[, dep] <-
          factor(tune.data[, dep], labels = make.names(levels(tune.data[, dep])))
        control <-
          trainControl(
            method = "boot",
            number = 10,
            search = "random",
            summaryFunction = twoClassSummary,
            classProbs = T
          )
        set.seed(1)
        grid <-
          expand.grid(
            fL = c(0, 0.5, 1.0),
            usekernel = TRUE,
            adjust = c(0, 0.5, 1.0)
          )
        nb_random <-
          train(
            f,
            data = tune.data,
            method = "nb",
            metric = "ROC",
            tuneGrid = grid,
            trControl = control
          )
        # Use the best fL and adjust
        classifier.params$nb.fL <- nb_random$bestTune$fL
        classifier.params$nb.adjust <- nb_random$bestTune$adjust
      }
      
      m <- train(
        f,
        data = training.data,
        method = "nb",
        trControl = trainControl(method = "none"),
        tuneGrid = data.frame(
          fL = classifier.params$nb.fL,
          usekernel = TRUE,
          adjust = classifier.params$nb.adjust
        )
      )
      
      # Permutation Importance for Naive Bayes
      genericVarImp <- c()
      for (predictorName in indep) {
        if (no.testing) {
          # if no testing data, use training data instead
          shuffledData <- training.data[, indep]
        } else {
          shuffledData <- testing.data[, indep]
        }
        shuffledData[, predictorName] <-
          sample(shuffledData[, predictorName], length(shuffledData[, predictorName]))
        predictions <-
          predict(m, newdata = shuffledData, type = "prob")[, "TRUE"]
        predictions <-
          ifelse(predictions > prob.threshold, TRUE, FALSE)
        if (no.testing) {
          # if no testing data, use training data instead
          genericVarImp <-
            cbind(genericVarImp, mean(training.data[, dep] != predictions))
        } else {
          genericVarImp <-
            cbind(genericVarImp, mean(testing.data[, dep] != predictions))
        }
      }
      colnames(genericVarImp) <- indep
      importance <-
        rbind(importance, c(genericVarImp))
      
      if (no.testing) {
        prob <-
          predict(m, testing.data, type = "prob")[, "TRUE"]
        
      }
      
    }  else if (classifier == "svm") {
      # set default gamma
      classifier.params$svm.gamma <- 1 / length(indep)
      
      if (params.tuning) {
        # Tuning gamma (sigma) and C - Random Search
        # 10 random and 10 bootstrap validation
        tune.data <- training.data
        tune.data[, dep] <-
          factor(tune.data[, dep], labels = make.names(levels(tune.data[, dep])))
        control <-
          trainControl(
            method = "boot",
            number = 10,
            search = "random",
            summaryFunction = twoClassSummary,
            classProbs = T
          )
        set.seed(1)
        svm_random <-
          train(
            f,
            data = tune.data,
            method = "svmRadial",
            metric = "ROC",
            tuneLength = 10,
            trControl = control
          )
        # Use the best gamma and C
        classifier.params$svm.gamma <- svm_random$bestTune$sigma
        classifier.params$svm.cost <- svm_random$bestTune$C
      }
      
      m <-
        svm(
          f,
          data = training.data,
          probability = TRUE,
          kernal = 'radial',
          gamma = classifier.params$svm.gamma,
          cost = classifier.params$svm.cost
        )
      
      # TODO
      importance <-
        rbind(importance, c(rep(0, length(indep))))
      
      if (no.testing) {
        prob <- predict(m, testing.data, probability = TRUE)
        prob <- attr(prob, "probabilities")[, 'TRUE']
        names(prob) <- row.names(testing.data)
      }
    }
    
    # Compute performance
    
    if (no.testing) {
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
                c(performance.calculation(
                  factor(testing.data[, dep]), prob, prob.threshold
                )))
      } else {
        performance <-
          rbind(performance,
                c(
                  performance.calculation(testing.data[, dep], prob, prob.threshold)
                ))
      }
    }
    
    importance <- data.frame(importance)
    names(importance) <- indep
    return(list(
      performance = data.frame(performance),
      importance = importance,
      model = m
    ))
  }
