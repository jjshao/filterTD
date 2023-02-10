#' A summary of what points are removed given the current filtering strategy.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param percentage Percentage of data you are willing to lose, default to NULL
#' @param hpem_max Maximum HPEm value you are willing to have the data be, default to NULL
#' @param df_return Default to FALSE, if TRUE then returns dataframe of all removed points. Otherwise, check return statement.
#' @return If df_return is FALSE, returns list of three return values (total number of removed points, number of removed points by tag, number of removed points by week).
#' If df_return is TRUE, returns dataframe of all removed points.
#' @examples
#' sync_rempoint <- removed.points(sync_tags=sync_tag_data, hpe_col_name="HPE",
#'   hpem_col_name="HPEm", id_col_name="Id",
#'   time_col_name="Time", percentage=0.1,
#'   df_return=FALSE)
#'
#' @export

removed.points <- function(sync_tags, hpe_col_name, hpem_col_name, id_col_name,
                           time_col_name, percentage=NULL, hpem_max=NULL, df_return=FALSE) {
  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))
  sync_tags$HPEm <- sync_tags[, hpem_col_name]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  if(missing(percentage) & !missing(hpem_max)) {
    sync_tags$prob <- sync_tags[, hpem_col_name]
    removed_points <- subset(sync_tags, sync_tags[, "prob"]>hpem_max)
  } else if(!missing(percentage) & missing(hpem_max)) {
    removed_points <- sync_tags[order(sync_tags$HPEbin),]
    n <- ceiling(nrow(sync_tags)*percentage)
    removed_points <- tail(removed_points, n)
  }

  if(df_return==TRUE) {
    return(removed_points)
  } else {
    # Total number of removed points
    total <- nrow(removed_points)

    # Number of points removed by tag
    bytag <- table(removed_points[id_col_name])
    bytag <- data.frame(bytag)

    # Number by time period
    # Add column to show week number
    tags_subset[,"week_num"] <- strftime(tags_subset[, time_col_name], format = "%V")
    # Count number of times these occur
    # Ensure that tag data date column are characters
    tags_subset[, time_col_name] <- as.character(tags_subset[, time_col_name])
    # Split into date and time
    tags_subset[, "date"] <- sapply(sapply(tags_subset[, time_col_name],function(x)strsplit(x," ")),function(x)x[1])
    tags_subset[, "time"] <- sapply(sapply(tags_subset[, time_col_name],function(x)strsplit(x," ")),function(x)x[2])
    # Change to be in R Date
    tags_subset[, "date"] <- as.Date(tags_subset[, "date"],format='%Y-%m-%d')
    num_obs_per_time <- table(tags_subset[, "week_num"])
    bytime <- data.frame(num_obs_per_time)
    names(bytime)[names(bytime) == 'Var1'] <- 'Week of the Year'

    # Number by place
    byplace <- table(removed_points[id_col_name])

    return(list(total, bytag, bytime))
  }

}
