#' Get VarClus based on the absolute Spearman correlation coefficients between metrics
#'
#' This function makes life simple by providing a VarClus.
#' @param dataset  a data frame for data
#' @param indep  a vector of characters or a vector of characters for independent variables
#' @param similarity a character for similarity measures (e.g., Spearman rank correlation), default = spearman
#' @param varclus.threshold a numeric for correlation coefficient threshold value
#' @import Hmisc
#' @keywords VarClus
#' @export
get.vc <- function(dataset, indep, similarity = 'spearman', varclus.threshold = 0.7){
  vc <-
    varclus(~ .,
            similarity = similarity,
            data = dataset[, indep],
            trans = "abs")
  vc$threshold <- varclus.threshold
  vc$indep <- indep
  vc$dataset <- dataset
  class(vc) <- c('summarizedvc')
  return(vc)
}
