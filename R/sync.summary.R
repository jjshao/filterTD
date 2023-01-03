#' Get summary stats for sync tags at chosen HPE levels
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_levels Vector of HPE levels
#' @return Vector of numerics of summary statistics for sync data, order: (mean, maximum, minimum, standard deviation, standard error)
#' @examples
#' num_obs_per_tag_lob <- num.obs.per.tag(animal_merged, "Id", graph=TRUE,
#' sp="Homarus americanus", sp_col_name="Sp")
#'
#' @export

sync.summary <- function(sync_tags, hpe_levels) {
  # table
  df <- data.frame(matrix(ncol = 6, nrow = 0))

  for (x in hpe_levels) {
    #If we choose HPE <= 25
    sync1 <- subset(sync_tag_data, HPE<=x)
    # create HPE bins
    sync1$HPEbin <- as.factor(round(sync1$HPE))
    avg <- mean(sync1$HPE) # *y value = X m
    maximum <- max(sync1$HPE) # *y value = X m
    minimum <- min(sync1$HPE) # *y value = X m
    std.dev <- sd(sync1$HPE) # *y value = X m
    std.err <- sd(sync1$HPE) / sqrt(length(sync1$HPE))
    # vector
    v <- c(x, avg, maximum, minimum, std.dev, std.err)
    print(v)
    # add row to table
    df <- rbind(df,v)
  }

  colnames(df) <- c("HPE", "mean", "maximum", "minimum", "standard deviation", "standard error")

  return(df)
}
