#' Get summary statistics for sync tags at chosen HPE values.
#' Summary statistics includes mean, maximum, minimum, standard deviation, and standard error.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpe_levels Vector of HPE values
#' @return Dataframe of numerics of summary statistics for sync data
#'
#' @examples
#' # Load data
#' sample_file <- system.file("extdata", "dummy_sync.csv", package = "filterTelemetry")
#' sync_tag_data <- read.table(file = sample_file, header=TRUE, sep=",")
#' # Example
#' sync_summary <- sync_summary(sync_tag_data,"HPE", c(25, 50))
#'
#'
#' @export
#'
#' @importFrom stats quantile

sync_summary <- function(sync_tags, hpe_col_name, hpe_levels) {
  # Create dataframe
  df <- data.frame(matrix(ncol = 6, nrow = 0))

  # Loop through vector of HPE values
  for (x in hpe_levels) {
    sync1 <- subset(sync_tags, sync_tags[,hpe_col_name]<=x)
    # Create HPE bins
    sync1$HPEbin <- as.factor(round(sync1[, hpe_col_name]))
    avg <- mean(sync1[, hpe_col_name])
    maximum <- max(sync1[, hpe_col_name])
    minimum <- min(sync1[, hpe_col_name])
    std.dev <- sd(sync1[, hpe_col_name])
    std.err <- sd(sync1[, hpe_col_name])/sqrt(length(sync1[, hpe_col_name]))
    # Vector of HPE value and statistics
    v <- c(x, avg, maximum, minimum, std.dev, std.err)
    # Add vector as row to dataframe
    df <- rbind(df,v)
  }

  # Rename dataframe column names
  colnames(df) <- c("HPE", "mean", "maximum", "minimum", "standard deviation", "standard error")

  return(df)
}
