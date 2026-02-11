#' A summary of what points are removed given the current filtering strategy.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param id_col_name String name of column in sync_tags that has tag ID
#' @param time_col_name String, name of column that includes date/time data, default to NULL
#' @param date_format Character string giving a date-time format as used in base as.POSIX, default to "%Y/%m/%d %H:%M:%OS"
#' @param percentage Percentage of data you are willing to lose, default to NULL
#' @param hpem_max Maximum HPEm value you are willing to have the data be, default to NULL
#' @param df_return Default to FALSE, if TRUE then returns dataframe of all removed points. Otherwise, check return statement.
#' @return If df_return is FALSE, returns list of three return values (total number of removed points, number of removed points by tag, number of removed points by week).
#' If df_return is TRUE, returns dataframe of all removed points.
#' @examples
#' # Load data
#' sample_file <- system.file("extdata", "dummy_sync.csv", package = "filterTelemetry")
#' sync_tag_data <- read.table(file = sample_file, header=TRUE, sep=",")
#' # Example
#' sync_rempoint <- removed_points(sync_tags=sync_tag_data, hpe_col_name="HPE",
#'   hpem_col_name="HPEm", id_col_name="Id",
#'   time_col_name="Time", percentage=0.1,
#'   df_return=FALSE)
#'
#' @export
#'
#' @importFrom utils tail
#' @importFrom stats aggregate

removed_points <- function(sync_tags, hpe_col_name, hpem_col_name, id_col_name,
                           time_col_name, date_format="%Y/%m/%d %H:%M:%OS", percentage=NULL, hpem_max=NULL, df_return=FALSE) {
  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[[hpe_col_name]]))
  sync_tags$HPEm <- sync_tags[[hpem_col_name]]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  if(missing(percentage) & !missing(hpem_max)) {
    sync_tags$prob <- sync_tags[[hpem_col_name]]
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
    bytag <- table(removed_points[[id_col_name]])
    bytag <- data.frame(bytag)
    names(bytag)[names(bytag) == 'Var1'] <- 'Transmitter ID'
    names(bytag)[names(bytag) == 'Freq'] <- 'Number Removed'

    # Number by time period
    # Add column to show week number
    removed_points[[time_col_name]] <- as.POSIXct(removed_points[[time_col_name]],
                                                  format = date_format)
    removed_points$week_num <- strftime(removed_points[[time_col_name]], format = date_format)
    # Count number of times these occur
    # Ensure that tag data date column are characters
    removed_points[[time_col_name]] <- as.character(removed_points[[time_col_name]])
    # Split into date and time
    removed_points$date <- sapply(sapply(removed_points[[time_col_name]],function(x)strsplit(x," ")),function(x)x[1])
    removed_points$time <- sapply(sapply(removed_points[[time_col_name]],function(x)strsplit(x," ")),function(x)x[2])
    # Change to be in R Date
    removed_points$date <- as.Date(removed_points$date, format = date_format)
    num_obs_per_time <- table(removed_points$week_num)
    bytime <- data.frame(num_obs_per_time)
    names(bytime)[names(bytime) == 'Var1'] <- 'Week of the Year'
    names(bytime)[names(bytime) == 'Freq'] <- 'Number Removed'

    return(list(total, bytag, bytime))
  }

}
