#' Check for constant metrics and categorical metrics
#' 
#' @param dataset  a data frame for data
#' @param metrics  a characters or a vector of characters for independent variables
#' @keywords constant categorical
#' @examples
#' Data = loadDefectDataset('groovy-1_5_7','jira')
#' check.constant.categorical(dataset = Data$data, metrics = Data$indep)
#' @export
check.constant.categorical <-
  function(dataset,
           metrics) {
    # Check constant metrics
    constant <-
      apply(dataset[, metrics], 2, function(x)
        max(x) == min(x))
    constant <- names(constant[constant == TRUE])
    # Remove constant metrics
    if (length(constant) > 0) {
      metrics <- metrics[!metrics %in% constant]
    }
    
    # Check categorical metrics
    category <- sapply(dataset[, metrics], class)
    category <- names(category[category == "character"])
    # Remove categorical metrics from Spearman Analysis
    if (length(category) > 0) {
      metrics <- metrics[!metrics %in% category]
    }
    
    return(metrics)
  }