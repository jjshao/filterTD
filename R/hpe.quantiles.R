# Function to..

hpe.quantiles <- function(sync_tags, quantiles=NULL) {
  if (missing(quantiles)) {
    quantiles <- c(0.95, 0.9, 0.75, 0.5, 0.25)

    # Create HPE bins, save in new column in sync_tag_data
    sync_tags$HPEbin <- as.factor(round(sync_tags$HPE))
    print(sync_tags$HPEbin)

    # Count number of detections per HPE value
    bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)
    print(head(bins))

    #loop.filter(sync_tags, quantiles)
    for (x in quantiles) {
      sync_prob <- as.data.frame(with(sync_tags, (tapply(HPEm, HPEbin,
                                                         quantile, probs=x,
                                                         na.rm=TRUE))))
      print(sync_prob)
    }

  } else {
    print("not missing")
  }
  return(sync_prob)
}
