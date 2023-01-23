#' Calculate linear regression given HPE level or percentage of data.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param probability
#' @param hpem_min
#' @return Vector of HPE values to cut off at for each quantile.
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
    sync_prob <- subset(sync_tags, sync_tags[, "HPEm"]<=hpem_min)

  } else if(!missing(probability) & missing(hpem_min)) {
    # probability 0.1 means willing to lose 10% of data
    sync_prob <- as.data.frame(with(sync_tags, (tapply(HPEm, HPEbin,
                                                       quantile, probs=probability,
                                                       na.rm=TRUE))))
    # Rename column
    colnames(sync_prob) <- c("Q95")
  }

  # percent remaining
  perc_remain <- nrow(sync_prob)/nrow(sync_tags)

  # Create a HPEbin column to merge
  sync_prob$HPEbin <- row.names(sync_prob)

  # Merge two datasets
  sync_ALL <- merge(bins, sync_prob, by="HPEbin")
  # Rename
  #sync1_ALL <- sync_ALL
  #sync1_ALL <- sync1_ALL %>% rename(Q95 = sync_prob)
  mod <- lm(Q95~as.numeric(HPEbin)-1, data=sync_ALL)
  print(summary(mod))

  print(ggplot(sync_ALL, aes(as.numeric(HPEbin), y=Q95))+
    geom_point()+ labs(c(x="Binned HPE", y = "95th quantile HPEm"))+ theme_bw()+
    geom_smooth(method="lm", formula=y~0+x)+
    annotate(label = sprintf("y = %.3f x\nR? = %.2f", coef(mod), summary(mod)$r.squared),
             geom = "text", x = 5, y = 30, size = 6)
)

}
