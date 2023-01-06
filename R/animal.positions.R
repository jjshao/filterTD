#' Graph
#'
#' @param animal_tags
#' @param ani_hpe_col_name
#' @param lat_col_name
#' @param long_col_name
#' @param num_bins
#' @param ID
#' @return Scatterplot
#' @examples
#'
#' @export
#'

require(scales)

animal.positions <- function(animal_tags, ani_hpe_col_name,
                             lat_col_name, long_col_name, num_bins, ID) {
  # Set up graph size
  par(mfrow = c(1,1), xpd=TRUE, mar = c(5, 4, 4, 8))
  # Create colour gradient
  rbPal <- colorRampPalette(c('red','blue'))

  # Filter to only keep all the rows of a certain tag
  tags_subset <- subset(ani_copy, ani_copy[, "Id"] == ID)
  # Create HPE bins, save in new column in sync_tag_data
  tags_subset$HPEbin <- as.factor(round(tags_subset[, "HPE"]))
  # Column of color values based on the HPEbin
  tags_subset$Col <- rbPal(num_bins)[cut(as.numeric(tags_subset$HPEbin), breaks=num_bins)]
  # Show breaks in legend
  cuts <- levels(cut(as.numeric(tags_subset$HPEbin), breaks=num_bins))

  # Plot
  plot(tags_subset[, long_col_name], tags_subset[, lat_col_name],
       main="Animal Tracks Classed by HPE, ID:",
       xlab="Longitude", ylab="Latitude ",
       col=alpha(tags_subset$Col,0.3), pch=19)
  # Text for ID, written in margin of each graph
  mtext(text = ID, side = 3, adj = 0.88, padj = -1.5, cex = 1.5)
  # Legend
  cuts <- gsub(",", " - ", cuts)
  cuts <- gsub("\\(", "[", cuts)
  legend("topright", inset = c(- 0.25, 0), cuts, col=rbPal(num_bins), pch=16)
}
