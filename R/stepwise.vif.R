#' Automated Stepwise Variance Inflation Factor Analysis
#'
#' This function automatically select non-correlated metrics based on a Variance Inflation Factor analysis. To do so, we exclude the metric that has the highest VIF score above a threshold value (default = 5) since the metric is the most predictable by others. We repeat an application of VIF analysis on the remaining metrics until all remaining metrics have their VIF scores below a threshold value and free from multicollinearity.
#'
#' @param dataset  a data frame for data
#' @param metrics  a characters or a vector of characters for independent variables
#' @param vif.threshold a numeric for a threshold of VIF score (default = 5)
#' @param verbose  TRUE for printing
#' @importFrom rms vif
#' @importFrom stats as.formula lm rnorm
#' @keywords VIF
#' @export
stepwise.vif <-
  function (dataset,
            metrics,
            vif.threshold = 5,
            verbose = F)
  {
    dataset$dummy <- rnorm(nrow(dataset))
    output <- metrics
    step.count <- 1
    output.results <- list()
    repeat {
      vif.scores <- vif(lm(as.formula(paste0(
        "dummy~", paste0(output,
                         collapse = "+")
      )), data = dataset))
      na.coefficients <- Reduce('|', is.nan(vif.scores))
      if (na.coefficients) {
        stop("NA coefficient in a regression model.")
      }
      output.results[[step.count]] <-
        sort(vif.scores, decreasing = F)
      vif.scores <- vif.scores[vif.scores >= vif.threshold]
      if (length(vif.scores) == 0)
        break
      drop.var <-
        names(vif.scores[vif.scores == max(vif.scores)])[1]
      if (verbose) {
        print(paste0(
          "Step ",
          step.count,
          " - Exclude ",
          drop.var,
          " (VIF = ",
          max(vif.scores),
          ")"
        ))
      }
      step.count <- step.count + 1
      output <- output[!output %in% drop.var]
    }
    names(output.results) <- paste0("Iteration ", 1:step.count)
    names(output.results)[length(output.results)] <- "Final"
    return(output)
  }
