#' Function that outputs a scatterplot of all observations of one ID (animal)
#' with longitude as the x-axis and latitude as the y-axis and the colour of
#' each point represents HPE as well as a barplot with the number of observations
#' of each HPE value.
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
require(gridExtra)

animal.positions <- function(animal_tags, ani_hpe_col_name,
                             lat_col_name, long_col_name, num_bins, ID,
                             id_col_name) {

  # Filter to only keep all the rows of a certain tag
  tags_subset <- subset(animal_tags, animal_tags[, id_col_name] == ID)
  # Create HPE bins, save in new column in sync_tag_data
  tags_subset$HPEbin <- as.factor(round(tags_subset[, ani_hpe_col_name]))

  # Create the ggplot scatterplot
  plot1 <- ggplot(tags_subset, aes(x = tags_subset[, long_col_name], y = tags_subset[, lat_col_name], color = cut(as.numeric(HPEbin), breaks=num_bins))) +
    geom_point(alpha = 0.3, size = 1, shape = 19) +
    scale_color_manual(values = colorRampPalette(c("red", "blue"))(num_bins)) +
    labs(title = paste("Animal Tracks Classed by HPE for Tag:", ID, fill=tags_subset$Col),
         x = "Longitude", y = "Latitude") +
    labs(color = "HPE Values") +
    theme_minimal()

  # Extract the summary statistics
  summary_stats <- as.data.frame(table(tags_subset$HPEbin))
  # Find the maximum count
  max_count <- max(summary_stats$Freq)
  # Make plot
  plot2 <- ggplot(tags_subset, aes(x=tags_subset[, hpe_col_name])) +
    geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    #scale_x_continuous(breaks = seq(min(tags_subset[, hpe_col_name]), max(tags_subset[, hpe_col_name]), by = 5)) +
    geom_vline(xintercept = median(tags_subset[, hpe_col_name]), color = "blue", linewidth=1) +
    labs(title = paste("Number of Observations per HPE Value for Tag", ID), x="HPE", y="Number of Observations") +
    annotate("text", x = mean(tags_subset[, hpe_col_name]), y = max_count, label = paste("HPE = ", median(tags_subset[, hpe_col_name])),
             color = "blue", hjust = 0) +
    theme(plot.title = element_text(size=15))

  grid.arrange(plot1, plot2, ncol = 2)
}
