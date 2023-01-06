#' Graph
#'
#' @param sync_tags
#' @param sync_hpe_col_name
#' @param animal_tags
#' @param ani_hpe_col_name
#' @param lat_col_name
#' @param long_col_name
#' @param num_bins
#' @return Scatterplot
#' @examples
#'
#' @export
#'

require(scales)

receiver.detections <- function(sync_tags=NULL, sync_hpe_col_name=NULL,
                                animal_tags=NULL, ani_hpe_col_name=NULL,
                                lat_col_name, long_col_name, num_bins) {
  # Set up graph size
  par(mfrow = c(1,1), xpd=TRUE, mar = c(5, 4, 4, 8))
  # Create colour gradient
  rbPal <- colorRampPalette(c('red','blue'))

  if(!missing(sync_tags) & !missing(sync_hpe_col_name)) {
    # Create HPE bins, save in new column in sync_tag_data
    sync_tags$HPEbin <- as.factor(round(sync_tags[, sync_hpe_col_name]))
    # Column of color values based on the HPEbin
    sync_tags$Col <- rbPal(num_bins)[cut(as.numeric(sync_tags$HPEbin), breaks=num_bins)]
    # Show breaks in legend
    cuts <- levels(cut(as.numeric(sync_tags$HPEbin), breaks=num_bins))
    # Plot
    plot(sync_tags[, long_col_name], sync_tags[, lat_col_name],
         main="Receivers Classed by HPE",
         xlab="Longitude", ylab="Latitude ",
         col=alpha(sync_tags$Col,0.3), pch=19)
    # Legend
    cuts <- gsub(",", " - ", cuts)
    cuts <- gsub("\\(", "[", cuts)
    legend("topright", inset = c(- 0.25, 0), cuts, col=rbPal(num_bins), pch=16)
  }
  if(!missing(animal_tags) & !missing(ani_hpe_col_name)) {
    # Create HPE bins, save in new column in sync_tag_data
    animal_tags$HPEbin <- as.factor(round(animal_tags[, ani_hpe_col_name]))
    # Column of color values based on the HPEbin
    animal_tags$Col <- rbPal(num_bins)[cut(as.numeric(animal_tags$HPEbin), breaks=num_bins)]
    # Show breaks in legend
    cuts <- levels(cut(as.numeric(animal_tags$HPEbin), breaks=num_bins))
    plot(animal_tags[, long_col_name], animal_tags[, lat_col_name],
         main="Animals Classed by HPE",
         xlab="Longitude", ylab="Latitude ",
         col=alpha(animal_tags$Col,0.3), pch=19)
    # Legend
    cuts <- gsub(",", " - ", cuts)
    cuts <- gsub("\\(", "[", cuts)
    legend("topright", inset = c(- 0.25, 0), cuts, col=rbPal(num_bins), pch=16)
  }
}
