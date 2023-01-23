#' Get HPE quantiles corresponding to the given or default probabilities.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param quantiles Vector of numerics, maximum 6 doubles, default to NULL. When NULL, quantiles = c(0.95, 0.9, 0.75, 0.5, 0.25)
#' @return Vector of HPE values to cut off at for each quantile.
#' @examples
#' quantiles <- c(0.9, 0.8, 0.5, 0.3, 0.1, 0)
#' sync_hpe2 <- hpe.quantiles(sync_tag_data, "HPE", quantiles)
#'
#' @export

hpe.quantiles <- function(sync_tags, hpe_col_name, hpem_col_name, quantiles=NULL) {

  if (missing(quantiles)) {
    quantiles <- c(0.95, 0.9, 0.75, 0.5, 0.25)
  } else {
    # Check to make sure vector of quantiles is no longer than 6 doubles
    len_quantiles <- length(quantiles)
    stopifnot(len_quantiles<=6)
    stopifnot(typeof(quantiles)=="double")
  }

  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))
  sync_tags$HPEm <- sync_tags[, hpem_col_name]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  # Adjust parameters of graph
  par(mfrow = c(2,3))

  # Loop through vector of quantiles
  for (x in quantiles) {
    sync_prob <- as.data.frame(with(sync_tags, (tapply(HPEm, HPEbin,
                                                       quantile, probs=x,
                                                       na.rm=TRUE))))

    # Rename column
    colnames(sync_prob) <- c("Q95")

    # Create a HPEbin column to merge
    sync_prob$HPEbin <- row.names(sync_prob)

    # Merge two datasets
    sync_ALL <- merge(bins, sync_prob, by="HPEbin")

    # Plot
    plot(as.numeric(sync_ALL[,1]), as.numeric(sync_ALL[,3]), main="HPE vs HPEm at Q",
         xlab="HPE", ylab="HPEm", pch=19)
    # Text for changing quantile, written in margin of each graph
    mtext(text = x, side = 3, adj = 0.95, padj = -1.35, cex = 1)
  }

  quantile_data <- quantile(sync_tags[, hpe_col_name], probs = quantiles)

  return(quantile_data)
}

