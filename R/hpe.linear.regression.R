#' Calculate linear regression given HPE level or percentage of data.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param probability
#' @param hpem_min
#' @return Vector of three values (p-value, r-squared, and percentage of removed data).
#' @examples
#'
#' @export

require(ggplot2)

hpe.linear.regression <- function(sync_tags, hpe_col_name, hpem_col_name,
                                  probability=NULL, hpem_min=NULL) {

  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))
  sync_tags$HPEm <- sync_tags[, hpem_col_name]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  if(missing(probability) & !missing(hpem_min)) {
    # hpem <= 2 means lose all data points that has hpem > 2
    sync_tags$prob <- sync_tags[, hpem_col_name]
    sync_prob <- subset(sync_tags, sync_tags[, "prob"]<=hpem_min)
  } else if(!missing(probability) & missing(hpem_min)) {
    # probability 0.1 means willing to lose 10% of data
    sync_prob <- as.data.frame(with(sync_tags, (tapply(HPEm, HPEbin,
                                                       quantile, probs=probability,
                                                       na.rm=TRUE))))
    # Rename column
    colnames(sync_prob) <- c("prob")
  }

  # percent remaining
  perc_remain <- nrow(sync_prob)/nrow(sync_tags)

  # Create a HPEbin column to merge
  sync_prob$HPEbin <- row.names(sync_prob)

  # Merge two datasets
  sync_ALL <- merge(bins, sync_prob, by="HPEbin")

  # Linear regression of HPEm (prob) and HPEbin
  mod <- lm(prob~as.numeric(HPEbin)-1, data=sync_ALL)

  print(ggplot(sync_ALL, aes(as.numeric(HPEbin), y=prob))+
    geom_point() + theme_bw() +
    geom_smooth(method="lm", formula=y~0+x)+
    annotate(label = sprintf("y = %.3f x\nR = %.2f", coef(mod), summary(mod)$r.squared),
             geom = "text", x = (max(as.numeric(sync_ALL$HPEbin))/2), y = (max(sync_ALL$prob)/2), size = 6,
             colour = "red") +
    labs(y = "HPEm", x = "Binned HPE", title = "Linear Regression of Binned HPE and HPEm"))

  return(c(summary(mod)$coefficients[,4], summary(mod)$r.squared, perc_remain))
}
