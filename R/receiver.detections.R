#' Graph
#'
#' @param sync_tags
#' @return Scatterplot
#' @examples
#' num_obs_per_tag_lob <- num.obs.per.tag(animal_merged, "Id", graph=TRUE,
#' sp="Homarus americanus", sp_col_name="Sp")
#'
#' @export
#'


receiver.detections.R <- function(sync_tags) {
  plot(sync_tags$Longitude, sync_tags$Latitude, main="Receivers Classed by HPE",
       xlab="Longitude", ylab="Latitude ", pch=19)
}
