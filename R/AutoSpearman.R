#' AutoSpearman - an automated variable selection approach based on the contemporary correlation analyses
#'
#' AutoSpearman is an automated metric selection approach based on a Spearman rank correlation test and a VIF analysis.
#' The approach is made up of two steps:
#' Step 1 - Automatically select non-correlated metrics based on a Spearman rank correlation test. To do so, we start from the pair of the strongest correlated metrics. Since these two metrics can be linearly predicted with each other, one of these two metrics must be removed while selecting the other. We select the metric that has the lowest Spearman correlation coefficient with the other metrics that are not in the pair. We repeat this process until all metrics have their Spearman correlation coefficient below a threshold value (default = 0.7).
#' Step 2 - Automatically select non-correlated metrics based on a Variance Inflation Factor analysis. To do so, we exclude the metric that has the highest VIF score above a threshold value (default = 5) since the metric is the most predictable by others. We repeat an application of VIF analysis on the remaining metrics until all remaining metrics have their VIF scores below a threshold value and free from multicollinearity.
#' Finally, AutoSpearman select only non-correlated metrics and produces a simpler non-correlated representative of all metrics.
#'
#' @param dataset  a data frame for data
#' @param metrics  a characters or a vector of characters for independent variables
#' @param spearman.threshold a numeric for a threshold of Spearman rank correlation test (default = 0.7)
#' @param vif.threshold a numeric for a threshold of VIF score (default = 5)
#' @param verbose  TRUE for printing
#' @keywords AutoSpearman
#' @examples 
#' Data = loadDefectDataset('groovy-1_5_7','jira')
#' AutoSpearman(dataset = Data$data, metrics = Data$indep)
#' @export
AutoSpearman <-
  function(dataset,
           metrics,
           spearman.threshold = 0.7,
           vif.threshold = 5,
           verbose = F) {
    
    # Check constant metrics
    constant <- apply(dataset[, metrics], 2, function(x) max(x) == min(x))
    constant <- names(constant[constant == TRUE])
    # Remove constant metrics
    if(length(constant) > 0){
      metrics <- metrics[!metrics %in% constant]
    }
    
    # Check categorical metrics
    category <- sapply(dataset[, metrics], class)
    category <- names(category[category=="character"])
    # Remove categorical metrics
    if(length(category) > 0){
      metrics <- metrics[!metrics %in% category]
    }
    
    spearman.metrics <- get.automated.spearman(dataset, metrics, spearman.threshold, verbose)
    AutoSpearman.metrics <- stepwise.vif(dataset, spearman.metrics, vif.threshold, verbose)

    return(AutoSpearman.metrics)
  }
