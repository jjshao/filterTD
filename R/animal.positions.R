#' Function that outputs a scatterplot of all observations of one ID (animal)
#' with longitude as the x-axis and latitude as the y-axis and the colour of
#' each point represents HPE.
#'
#' @name animal.positions
#' @param animal_tags Table of animal data
#' @param ani_hpe_col_name String name of column that has HPE values
#' @param lat_col_name String name of column that has latitude of observations
#' @param long_col_name String name of column that has longitude of observations
#' @param num_bins Integer number of bins that user wants HPE to be split into
#' @param ID Integer of animal ID
#' @examples
#' # load dataset:
#' dummy_animal <- read.table(file = './extdata/dummy_animals.csv', header=TRUE, sep=",")
#' animal.positions(animal_tags=dummy_animal, ani_hpe_col_name="HPE",
#'                    lat_col_name="Latitude", long_col_name="Longitude",
#'                    num_bins=6, ID=15048, id_col_name="Id")
#'
#' @export
#'

require(scales)
require(ggplot2)

animal.positions <- function(animal_tags, ani_hpe_col_name,
                             lat_col_name, long_col_name, num_bins, ID,
                             id_col_name) {
  # Set up graph size
  #par(mfrow = c(1,1), xpd=TRUE, mar = c(5, 4, 4, 8))
  par(mfrow = c(1,1))
  # Create colour gradient
  rbPal <- colorRampPalette(c('red','blue'))

  # Filter to only keep all the rows of a certain tag
  tags_subset <- subset(animal_tags, animal_tags[, id_col_name] == ID)
  # Create HPE bins, save in new column in sync_tag_data
  tags_subset$HPEbin <- as.factor(round(tags_subset[, ani_hpe_col_name]))
  # Column of color values based on the HPEbin
  tags_subset$Col <- rbPal(num_bins)[cut(as.numeric(tags_subset$HPEbin), breaks=num_bins)]
  # Show breaks in legend
  cuts <- levels(cut(as.numeric(tags_subset$HPEbin), breaks=num_bins))

  # Plot
  plot(tags_subset[, long_col_name], tags_subset[, lat_col_name],
       main="Animal Tracks Classed by HPE, ID:",
       xlab="Longitude", ylab="Latitude ",
       col=alpha(tags_subset$Col,0.3), pch=19, cex=0.4, xaxt='n', yaxt='n')
  # Axis
  max_long <- max(tags_subset[, long_col_name])
  min_long <- min(tags_subset[, long_col_name])
  max_lat <- max(tags_subset[, lat_col_name])
  min_lat <- min(tags_subset[, lat_col_name])
  axis(side=1, at=seq(min_long, max_long, by=(max_long - min_long)/10), las=2,
       cex.axis=0.65, tck = 1, lty = 1, col = "gray")
  axis(side=2, at=seq(min_lat, max_lat, by=(max_lat - min_lat)/10), las=2,
       cex.axis=0.65, tck = 1, lty = 1, col = "gray")
  # Text for ID, written in margin of each graph
  mtext(text = ID, side = 3, adj = 0.88, padj = -1.5, cex = 1.5)
  # Legend
  cuts <- gsub(",", " - ", cuts)
  cuts <- gsub("\\(", "[", cuts)
  legend("topright", cuts, col=rbPal(num_bins), pch=16)

  ggplot(tags_subset, aes(x=tags_subset[, hpe_col_name])) +
    geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    #scale_x_continuous(breaks = seq(min(tags_subset[, hpe_col_name]), max(tags_subset[, hpe_col_name]), by = 5)) +
    geom_vline(xintercept = median(tags_subset[, hpe_col_name]), color = "blue", size=1) +
    labs(title = "crabe_57016", caption = paste("filtered by HPE < 60", sep = "")) +
    annotate("text", x = 45, y = 9500, label = "HPE = 38.07\nQuantile 95%\n",
             color = "blue") +
    theme(
      plot.title = element_text(size=15)
    )
}
