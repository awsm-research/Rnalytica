
#' @import dendextend reshape2
#' @importFrom graphics abline axis locator par plot strwidth text title
#' @importFrom stats as.dendrogram
#' @export
plot.summarizedvc <- function(vc,
                              legend = c('Non-correlated metrics', 'Correlated metrics'),
                              abbrev = FALSE,
                              legend. = FALSE,
                              loc,
                              maxlen = 20,
                              labels = NULL,
                              ...){

  sim <- melt(vc$sim)
  correlated.sim <- sim[sim$value >= vc$threshold, ]
  correlated.metrics <- vc$indep[vc$indep %in% unique(sort(c(as.character(correlated.sim$Var1), as.character(correlated.sim$Var2))))]
  trans <- vc$trans
  s <- c(
    hoeffding = "30 * Hoeffding D",
    spearman = switch(
      trans,
      square = expression(paste(Spearman, ~
                                  rho ^ 2)),
      abs = expression(paste(Spearman,
                             ~
                               abs(rho))),
      none = expression(paste(Spearman,
                              ~
                                rho))
    ),
    pearson = switch(
      trans,
      square = expression(paste(Pearson,
                                ~
                                  r ^ 2)),
      abs = expression(paste(Pearson, ~ abs(r))),
      none = expression(paste(Pearson, ~
                                r))
    ),
    bothpos = "Proportion",
    ccbothpos = "Chance-Corrected Proportion"
  )[vc$similarity]
  if ((is.expression(s) &&
       as.character(s) == "NULL") ||
      (!is.expression(s) && (is.na(s) || s == "")))
    s <- vc$similarity
  ylab <- s

  if (legend.)
    abbrev <- TRUE
  if (!length(labels))
    labels <- dimnames(vc$sim)[[2]]
  olabels <- labels
  if (abbrev)
    labels <- abbreviate(labels)
  if (!length(vc$hclust))
    stop("clustering was not done on similarity=\"ccbothpos\"")

  # Modify from
  # https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html
  # set3 ####
  dend <- as.dendrogram(vc$hclust, hang = -1)
  ifill = c('darkgreen', 'red')
  text.color <-
    ifelse(labels(dend) %in% correlated.metrics, 'red', 'darkgreen')
  dend <-
    assign_values_to_leaves_edgePar(dend = dend,
                                    value = text.color,
                                    edgePar = "col")

  # max pixels need to plot metrics
  max.length <-
    max(unlist(lapply(vc$indep, function(x)
      strwidth(x, font = 12, units = 'in'))))
  tick.margin <- (5 + (4.544286 * (max.length - 0.574)))
  par(mar = c(5, 2.5, 5, tick.margin))
  #dend %>% hang.dendrogram(hang_height = 0.2) %>%
  plot(
    dend,
    main = "",
    ylab = '',
    yaxt = 'n',
    labels = labels,
    ann = FALSE,
    axes = FALSE,
    hang = -1,
    horiz = T
  )
  xa <- pretty(range(1 - vc$hclust$height))
  axis(1, at = 1 - xa, labels = format(xa))
  title(xlab = ylab)
  s <- labels != olabels
  if (legend. && any(s)) {
    if (missing(loc)) {
      cat("Click mouse at upper left corner of legend\n")
      loc <- locator(1)
    }
    olabels <- ifelse(nchar(olabels) > maxlen,
                      substring(olabels,
                                1, maxlen),
                      olabels)
    text(loc, paste(paste(labels[s], ":", olabels[s], "\n"),
                    collapse = ""), adj = 0)
  }
  abline(v = (1 - vc$threshold))
  legend(
    'top',
    legend = legend,
    fill = ifill,
    xpd = TRUE,
    inset = c(0, -0.1)
    # inset = c(0, (-0.1 - (
    #   0.006 * (bottomMargin - 5)
    # )))
  )
  # set2####
  invisible()

}


