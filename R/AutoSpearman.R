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
           groups = FALSE,
           verbose = F) {
    # Remove constant metrics and categorical metrics
    metrics <- remove.constant.categorical(dataset, metrics)
    
    if(groups == TRUE){
      metrics <- remove.constant.categorical(dataset, metrics)
      spearman_df_metrics <- get.automated.spearman_Group(dataset, metrics, 
                                                          spearman.threshold, verbose)
      AutoSpearman.metrics <- stepwise.vif(spearman_df_metrics$dataset, spearman_df_metrics$selected.metrics, 
                                           vif.threshold, verbose)
      
      return((lapply(AutoSpearman.metrics, function(y) strsplit(y, '__')[[1]])))
    }else{
      spearman.metrics <-
        get.automated.spearman(dataset, metrics, spearman.threshold, verbose)
      AutoSpearman.metrics <-
        stepwise.vif(dataset, spearman.metrics, vif.threshold, verbose)
      
      return(AutoSpearman.metrics)
    }
  }
