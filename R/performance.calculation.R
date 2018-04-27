#' Performance Calculation
#'
#' This function exports confusion matrix (i.e., true positive, false positive, true negative, and false negative) and calculates 11 performance measures.
#' The performance measures are AUC, Precision, Recall, False Positive Rate, True Negative Rate, F-measure, Accuracy, MCC, G-mean, G-measure, and Balance.
#'
#' @param outcome a vector of the outcome labels.
#' @param prob  a numeric vector of the predicted probability.
#' @param threshold a numeric threshold for the predicted probability. A predicted probability of above a threshold is considered a positive prediction.
#' @import pROC
#' @keywords performance
#' @export
performance.calculation <- function(outcome, prob, threshold=0.5){

  m1 <- c(AUC=as.numeric(auc(outcome, prob)))

  classify <- ifelse(prob > threshold, TRUE, FALSE)
  m2 <- table(classify,outcome)

  tp <- fp <- fn <- tn <- NULL
  if(length(table(classify)) == 1){
    tp <- 0; fp <- 0; fn <- m2[1,2]; tn <- m2[1,1]
  }else{
    tp <- m2[2,2]; fp <- m2[2,1]; fn <- m2[1,2]; tn <- m2[1,1]
  }

  recall <- tp/(tp+fn)
  precision <- tp/(tp+fp)
  fpr <- fp/(fp+tn)
  tnr <- tn/(tn+fp)
  fmeasure <- 2*precision*recall/(precision+recall)
  accuracy <- (tn+tp)/(tn+fn+fp+tp)

  mcc.prod <- log2(tp + fp) + log2(tp + fn) + log2(tn + fp) + log2(tn + fn)
  mcc <- (tp * tn - fp * fn) / sqrt(2 ^ mcc.prod)
  gmean <- sqrt(precision*recall)
  gmeasure <- 2*recall*(1-fpr)/(recall + (1-fpr))
  pf <- fp/(tn+fp) # probability of false alarm
  balance <- 1- (sqrt( (0-pf)^2 + (1-recall)^2)/sqrt(2) )

  out <- c(m1,Precision=precision,Recall=recall,FPR=fpr,TNR=tnr,Fmeasure=fmeasure,Accuracy=accuracy,MCC=mcc,Gmean=gmean,Gmeasure=gmeasure,Balance=balance, TP=tp, FP=fp, FN=fn, TN=tn)
  out
}
