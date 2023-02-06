#' A summary of what points are removed given the current filtering strategy.
#'
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param percentage
#' @param hpem_min
#' @return
#' @examples
#'
#' @export

removed.points <- function(sync_tags, hpe_col_name, hpem_col_name,
                                  percentage=NULL, hpem_min=NULL) {
  # Create HPE bins, save in new column in sync_tag_data
  sync_tags$HPEbin <- as.factor(round(sync_tags[, hpe_col_name]))
  sync_tags$HPEm <- sync_tags[, hpem_col_name]
  # Count number of detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)

  if(missing(percentage) & !missing(hpem_min)) {
    sync_tags$prob <- sync_tags[, hpem_col_name]
    removed_points <- subset(sync_tags, sync_tags[, "prob"]>hpem_min)
  } else if(!missing(percentage) & missing(hpem_min)) {
    removed_points <- sync_tags[order(sync_tags$HPEbin),]
    n <- ceiling(nrow(sync_tags)*percentage)
    removed_points <- tail(removed_points, n)
  }
  return(removed_points)
}
