#' Get HPE quantiles corresponding to the given or default probabilities.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param quantiles Vector of numerics, maximum 6 doubles, default to NULL. When NULL, quantiles = c(0.95, 0.9, 0.75, 0.5, 0.25)
#' @return Dataframe of HPE values to cut off at for each quantile.
#' @examples
#' # load dataset:
#' sync_tag_data <- read.table(file = './extdata/dummy_sync.csv', header=TRUE, sep=",")
#' quantiles <- c(0.9, 0.8, 0.5, 0.3, 0.1, 0)
#' sync_hpe2 <- hpe.quantiles(sync_tag_data, "HPE", "HPEm", quantiles)
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

  # Adjust parameters of graph
  par(mfrow = c(2,3))

  # Initialize a vector to store the number of points removed for each quantile
  points_removed <- numeric(length(quantiles))
  # Original number of total points
  orig_num <- nrow(sync_tags)

  # Calculate the HPE value for the quantiles
  quantile_data <- quantile(sync_tags[, hpe_col_name], probs = quantiles)

  # Variable for naming
  i = 1

  # Loop through vector of quantiles
  for (x in quantile_data) {
    # Segment sync_tags to keep only rows where the HPE is less than or equal to
    #the accepted HPE for that quantile
    remaining_tags <- sync_tags[sync_tags[, hpe_col_name] <= x, ]

    # Count the number of points removed
    points_removed[i] <- orig_num - nrow(remaining_tags)

    # Plot
    plot(as.numeric(remaining_tags$HPE), remaining_tags$HPEm, main="HPE vs HPEm at Q",
         xlab="HPE", ylab="HPEm", pch=19)
    # Text for changing quantile, written in margin of each graph
    mtext(text = quantiles[i], side = 3, adj = 0.95, padj = -1.35, cex = 1)

    i = i + 1
  }

  # Add in the number of points removed to the table of HPE values for each quantile
  result_df <- data.frame(Value = c(quantile_data),
                          "Number_Points_Removed" = points_removed)

  return(result_df)
}

