#' Calculate linear regression given HPE level or percentage of data.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param probability
#' @return Vector of HPE values to cut off at for each quantile.
#' @examples
#'
#' @export

hpe.linear.regression <- function(sync_tags, hpe_col_name, hpem_col_name, probability) {

  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))
  sync_tags$HPEm <- sync_tags[, hpem_col_name]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  sync_prob <- as.data.frame(with(sync_tags, (tapply(HPEm, HPEbin,
                                                     quantile, probs=probability,
                                                     na.rm=TRUE))))

  #sync_prob <- as.data.frame(sync_prob)
  # Rename column
  colnames(sync_prob) <- c("Q95")

  # Create a HPEbin column to merge
  sync_prob$HPEbin <- row.names(sync_prob)

  # Merge two datasets
  sync_ALL <- merge(bins, sync_prob, by="HPEbin")
  # Rename
  sync1_ALL <- sync_ALL
  #sync1_ALL <- sync1_ALL %>% rename(Q95 = sync_prob)
  mod <- lm(Q95~as.numeric(HPEbin)-1, data=sync1_ALL)
  print(summary(mod))

  ggplot(sync1_ALL, aes(as.numeric(HPEbin), y=Q95))+
    geom_point()+ labs(c(x="Binned HPE", y = "95th quantile HPEm"))+ theme_bw()+
    geom_smooth(method="lm", formula=y~0+x)+
    annotate(label = sprintf("y = %.3f x\nR? = %.2f", coef(mod), summary(mod)$r.squared),
             geom = "text", x = 5, y = 30, size = 6)


}
