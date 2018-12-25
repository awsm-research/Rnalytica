#' Get VarClus plot based on the absolute Spearman correlation coefficients between metrics
#'
#' This function makes life simple by providing a VarClus plot.
#' @param dataset  a data frame for data
#' @param metrics  a vector of characters or a vector of characters for independent variables
#' @param correlation a character for correlation measures (e.g., Spearman rank correlation), default = spearman
#' @param correlation.threshold a numeric for correlation coefficient threshold value
#' @keywords VarClus
#' @examples 
#' Data = loadDefectDataset('camel-1.0','ck')
#' plotVarClus(dataset = Data$data, metrics = Data$indep)
#' @export
plotVarClus <- function(dataset, metrics, correlation = 'spearman', correlation.threshold = 0.7){
  vc <- get.vc(dataset, metrics, similarity = correlation, varclus.threshold = correlation.threshold)
  plot(vc)
  # return(vc)
}
