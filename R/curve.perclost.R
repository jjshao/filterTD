#' Outputs a plot that shows the percentage of data remaining after filtering
#' by each HPE value.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpe_val Integer value for max HPE value user wants to display the graph to (with the max being the largest HPE value rounded to the nearest integer using base R round, IEC 60559 standard)
#' @examples
#' # Load data
#' sync_tag_data <- read.table(file = './extdata/dummy_sync.csv', header=TRUE, sep=",")
#' # Example
#' curve.perclost(sync_tags=sync_tag_data, hpe_col_name="HPE", hpe_val=20)
#'
#' @export

require(ggplot2)

curve.perclost <- function(sync_tags, hpe_col_name, hpe_val) {
  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))
  sync_tags$HPEm <- sync_tags[, hpem_col_name]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  uni_hpe_vals <- as.numeric(as.character(unique(sync_tags$HPEbin)))

  # Initialize a vector to store the number of points removed for each quantile
  points_removed <- numeric(length(uni_hpe_vals))
  # Original number of total points
  orig_num <- nrow(sync_tags)
  # Calculate and display the number of points removed for each quantile
  for (i in seq_along(uni_hpe_vals)) {
    q <- uni_hpe_vals[i]
    # Get the indices of points outside the range
    outside_indices <- which(sync_tags[, hpe_col_name] < q)
    # Get percentage of points removed
    points_removed[i] <- (1 - (orig_num - length(outside_indices))/orig_num) * 100
  }

  result_df <- data.frame(Value = c(uni_hpe_vals),
                          "Perc_Removed" = points_removed)
  # Plot
  print(ggplot(result_df[1:hpe_val,], aes(x=Value, y=Perc_Removed))+
          geom_line() + geom_point() + theme_bw() +
          labs(y = "Percentage of Data Remaining", x = "Binned HPE",
               title = "Percentage Data Remaining if Filtered by x-value of Binned HPE"))
}
