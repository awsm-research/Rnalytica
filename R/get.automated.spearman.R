#' Automated Spearman rank correlation test
#'
#' This function automatically select non-correlated metrics based on a Spearman rank correlation test. To do so, we start from the pair of the strongest correlated metrics. Since these two metrics can be linearly predicted with each other, one of these two metrics must be removed while selecting the other. We select the metric that has the lowest Spearman correlation coefficient with the other metrics that are not in the pair. We repeat this process until all metrics have their Spearman correlation coefficient below a threshold value (default = 0.7).
#'
#' @param dataset  a data frame for data
#' @param metrics  a characters or a vector of characters for independent variables
#' @param spearman.threshold a numeric for a threshold of Spearman rank correlation test (default = 0.7)
#' @param verbose  TRUE for printing
#' @importFrom Hmisc rcorr
#' @keywords Spearman


get.automated.spearman <- function(dataset, metrics, spearman.threshold, verbose = F){

  .get.higher.correlation <- function(index1, index2, metrics, metric.correlations, verbose = F, count){
    metric1 <- metrics[index1]
    metric2 <- metrics[index2]
    if(verbose)
      cat(paste0('Step ', count, ' - {', metric1, ', ', metric2, '} > '))# are correlated with r = ', metric.correlations[metric1, metric2], '\n'))
    metric1.correlation <- mean(metric.correlations[metric1, !metrics %in% c(metric1, metric2)])
    metric2.correlation <- mean(metric.correlations[metric2, !metrics %in% c(metric1, metric2)])
    if(metric1.correlation <= metric2.correlation){
      return(index2)
    } else {
      return(index1)
    }
  }

  metric.correlations <- abs(rcorr(as.matrix(dataset[, metrics]), type = 'spearman')$r)

  above.threshold <- which((metric.correlations >= spearman.threshold), arr.ind = TRUE)
  row.names(above.threshold) <- NULL
  above.threshold <- as.data.frame(above.threshold, row.names = NULL)
  above.threshold <- above.threshold[above.threshold$row != above.threshold$col, ]
  above.threshold$correlation <- 100
  for(i in 1:nrow(above.threshold)){
    above.threshold$correlation[i] <- metric.correlations[above.threshold$row[i], above.threshold$col[i]]
  }
  above.threshold <- above.threshold[order(-above.threshold$correlation), ]

  exclude.metrics <- {}
  count <- 1
  repeat{
    if(nrow(above.threshold) == 0)
      break
    tmp <- above.threshold[1, ]
    exclude.index <- .get.higher.correlation(tmp$row, tmp$col, metrics, metric.correlations, verbose, count)
    exclude.metrics <- c(exclude.metrics, metrics[exclude.index])
    if(verbose){
      cat(paste0(metrics[exclude.index], '\n'))
      count <- count + 1
    }
    above.threshold <- above.threshold[-which((above.threshold$row == exclude.index) | (above.threshold$col == exclude.index)), ]
  }
  selected.metrics <- metrics[!metrics %in% exclude.metrics]
  return(selected.metrics)
}

get.automated.spearman_Group <- function (dataset, metrics, spearman.threshold, verbose = F) 
{
  .get.higher.correlation <- function(index1, index2, metrics, 
                                      metric.correlations, verbose = F, count) {
    metric1 <- metrics[index1]
    metric2 <- metrics[index2]
    if (verbose) 
      cat(paste0("Step ", count, " - {", metric1, ", ", 
                 metric2, "} > "))
    metric1.correlation <- mean(metric.correlations[metric1, 
                                                    !metrics %in% c(metric1, metric2)])
    metric2.correlation <- mean(metric.correlations[metric2, 
                                                    !metrics %in% c(metric1, metric2)])
    if (metric1.correlation <= metric2.correlation) {
      return(index2)
    }
    else {
      return(index1)
    }
  }
  metric.correlations <- abs(rcorr(as.matrix(dataset[, metrics]), 
                                   type = "spearman")$r)
  above.threshold <- which((metric.correlations >= spearman.threshold), 
                           arr.ind = TRUE)
  row.names(above.threshold) <- NULL
  above.threshold <- as.data.frame(above.threshold, row.names = NULL)
  above.threshold <- above.threshold[above.threshold$row != 
                                       above.threshold$col, ]
  above.threshold$correlation <- 100
  for (i in 1:nrow(above.threshold)) {
    above.threshold$correlation[i] <- metric.correlations[above.threshold$row[i], 
                                                          above.threshold$col[i]]
  }
  above.threshold <- above.threshold[order(-above.threshold$correlation), 
                                     ]
  exclude.metrics <- {
  }
  count <- 1
  repeat {
    if (nrow(above.threshold) == 0) 
      break
    tmp <- above.threshold[1, ]
    exclude.index <- .get.higher.correlation(tmp$row, tmp$col, 
                                             metrics, metric.correlations, verbose, count)
    exclude.metrics <- c(exclude.metrics, metrics[exclude.index])
    
    # rename metric, correlation table, and df
    rc_index <- c(tmp$row, tmp$col)
    to_replace_index <- rc_index[rc_index != exclude.index]
    to_replace_metric <- metrics[rc_index[rc_index != exclude.index]]
    
    combined_metric <- paste0(metrics[rc_index], collapse = '__')
    metrics[to_replace_index] <- combined_metric
    row.names(metric.correlations)[row.names(metric.correlations) == to_replace_metric] <- combined_metric
    colnames(metric.correlations)[colnames(metric.correlations) == to_replace_metric] <- combined_metric
    names(dataset)[names(dataset) == to_replace_metric] <- combined_metric
    
    if (verbose) {
      cat(paste0(metrics[exclude.index], "\n"))
      count <- count + 1
    }
    above.threshold <- above.threshold[-which((above.threshold$row == 
                                                 exclude.index) | (above.threshold$col == exclude.index)), 
                                       ]
  }
  selected.metrics <- metrics[!metrics %in% exclude.metrics]
  return(list(dataset = dataset[, selected.metrics],
              selected.metrics = selected.metrics))
}