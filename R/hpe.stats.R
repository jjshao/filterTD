#' Get number of observations per tag when given all data or data is sorted
#' by species, sex, grouping, or a combination, can output graph. Grouping can
#' be used for a variety of different separations of data such as treatment.
#'
#' @param sync_tags Table of data
#' @param hpe_col_name String name of column that has tag IDs
#' @return Vector of number of total number of observations and total number of tags (for given parameters) and graph if graph=TRUE
#' @examples
#' num_obs_per_tag_lob <- num.obs.per.tag(animal_merged, "Id", graph=TRUE,
#' sp="Homarus americanus", sp_col_name="Sp")
#'
#' @export

hpe.stats <- function(sync_tags, hpe_col_name) {
  # Create HPE bins
  sync_tags$HPEbin <- as.factor(round(sync_tags$hpe_col_name))

  # count detections per HPE value
  bins <- aggregate(HPEm ~ HPEbin, sync_tags, length)
  head(bins)

  bins$HPEm_count <- bins$HPEm
  bins$HPEm <- NULL
  return(bins)
}
