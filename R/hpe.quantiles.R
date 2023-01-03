#' Get HPE quantiles
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param quantiles Default to NULL
#' @return
#' @examples
#' num_obs_per_tag_lob <- num.obs.per.tag(animal_merged, "Id", graph=TRUE,
#' sp="Homarus americanus", sp_col_name="Sp")
#'
#' @export

hpe.quantiles <- function(sync_tags, hpe_col_name, quantiles=NULL) {

  if (missing(quantiles)) {
    quantiles <- c(0.95, 0.9, 0.75, 0.5, 0.25)
  } else {
    len_quantiles <- length(quantiles)
    stopifnot(len_quantiles<=6)
  }

  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))

  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  # Adjust parameters of graph
  par(mfrow = c(2,3))

  #loop.filter(sync_tags, quantiles)
  for (x in quantiles) {
    sync_prob <- as.data.frame(with(sync_tags, (tapply(HPEm, HPEbin,
                                                       quantile, probs=x,
                                                       na.rm=TRUE))))

    sync_prob <- as.data.frame(sync_prob)

    # Create a HPEbin column to merge
    sync_prob$HPEbin <- row.names(sync_prob)

    # Merge two datasets
    sync_ALL <- merge(bins, sync_prob, by="HPEbin")

    plot(as.numeric(sync_ALL[,1]), as.numeric(sync_ALL[,3]), main="HPE vs HPEm at Q",
         xlab="HPE", ylab="HPEm", pch=19)
    mtext(text = x, side = 3, adj = 0.95, padj = -1.35, cex = 1)
  }
  quantile_data <- quantile(sync_tags$HPE, probs = quantiles)

  return(quantile_data)
}
