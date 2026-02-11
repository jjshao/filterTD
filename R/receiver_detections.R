#' Function that outputs a scatterplot of all observations of sync tags
#' with longitude as the x-axis and latitude as the y-axis and the colour of
#' each point represents HPE.
#'
#' @param sync_tags Table of data of sync tags
#' @param sync_hpe_col_name String name of column in sync_tags that has HPE values
#' @param animal_tags Table of data of animal tags
#' @param ani_hpe_col_name String name of column in animal_tags that has HPE values
#' @param lat_col_name String name of column in animal_tags that has latitude
#' @param long_col_name String name of column in animal_tags that has longitude
#' @param bins Vector of doubles of bins that user wants HPE to be split into, max vector length 6. If empty, defaults to c(4,6,10,50,250)
#' @param id_col_name String name of column in sync_tags that has tag ID
#' @examples
#' # Load data
#' sample_file <- system.file("extdata", "dummy_sync.csv", package = "filterTelemetry")
#' sync_tag_data <- read.table(file = sample_file, header=TRUE, sep=",")
#' # Example
#' receiver_detections(sync_tags=sync_tag_data, sync_hpe_col_name="HPE", bins=7,
#'                     lat_col_name="Latitude", long_col_name="Longitude", id_col_name="Id")
#'
#' @export
#'
#' @import scales
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics axis
#' @importFrom graphics legend
#'

receiver_detections <- function(sync_tags=NULL, sync_hpe_col_name=NULL,
                                animal_tags=NULL, ani_hpe_col_name=NULL,
                                lat_col_name, long_col_name, bins=NULL,
                                id_col_name) {
  if(is.null(bins)) {
    bins <- c(4,6,10,50,250)
  } else {
    # Check to make sure vector of quantiles is no longer than 6
    len_bins <- length(bins)
    stopifnot(len_bins<=6)
  }

  # Set up graph size
  par(mfrow = c(1,1))
  # Create colour gradient
  rbPal <- colorRampPalette(c('red','blue'))
  n_bins <- length(bins)

  if(!missing(sync_tags) & !missing(sync_hpe_col_name)) {
    # Create HPE bins, save in new column in sync_tag_data
    sync_tags$HPEbin <- as.factor(round(sync_tags[, sync_hpe_col_name]))
    # Column of color values based on the HPEbin
    sync_tags$Col <- rbPal(n_bins)[cut(as.numeric(sync_tags$HPEbin), breaks=bins)]
    # Show breaks in legend
    cuts <- levels(cut(as.numeric(sync_tags$HPEbin), breaks=bins))
    # Median
    split_sync <- split(sync_tags, sync_tags[, id_col_name])
    df = data.frame(matrix(nrow=0, ncol=2))
    colnames(df) = c("Latitude", "Longitude")
    num <- 1
    for(x in unique(sync_tags[, id_col_name])) {
      trial <- data.frame(split_sync[num])
      colnames(trial) <- colnames(sync_tags)
      med_lat <- median(trial[, lat_col_name])
      med_long <- median(trial[, long_col_name])
      df[nrow(df)+1,] <- c(med_lat, med_long)
      num <- num + 1
    }
    # Plot
    plot(sync_tags[, long_col_name], sync_tags[, lat_col_name],
         main="Receivers Classed by HPE",
         xlab="Longitude", ylab="Latitude ",
         col=alpha(sync_tags$Col,0.05), pch=19, cex=0.4, xaxt='n', yaxt='n')
    points(df[, "Longitude"], df[, "Latitude"], cex=0.4)
    # Axis
    max_long <- max(sync_tags[, long_col_name])
    min_long <- min(sync_tags[, long_col_name])
    max_lat <- max(sync_tags[, lat_col_name])
    min_lat <- min(sync_tags[, lat_col_name])
    xaxis <- round(seq(min_long, max_long, by=(max_long - min_long)/5), digits=2)
    yaxis <- round(seq(min_lat, max_lat, by=(max_lat - min_lat)/5), digits=2)
    axis(side=1, at=xaxis, las=2,
         cex.axis=0.65, tck = 1, lty = 1, col = "gray")
    axis(side=2, at=yaxis, las=2,
         cex.axis=0.65, tck = 1, lty = 1, col = "gray")
    # Legend
    cuts <- gsub(",", " - ", cuts)
    cuts <- gsub("\\(", "[", cuts)
    legend("topright", cuts, col = rbPal(n_bins), pch = 16)

  }
  if(!missing(animal_tags) & !missing(ani_hpe_col_name)) {
    # Create HPE bins, save in new column in sync_tag_data
    animal_tags$HPEbin <- as.factor(round(animal_tags[, ani_hpe_col_name]))
    # Column of color values based on the HPEbin
    animal_tags$Col <- rbPal(n_bins)[cut(as.numeric(animal_tags$HPEbin), breaks=bins)]
    # Show breaks in legend
    cuts <- levels(cut(as.numeric(animal_tags$HPEbin), breaks=bins))
    plot(animal_tags[, long_col_name], animal_tags[, lat_col_name],
         main="Animals Classed by HPE",
         xlab="Longitude", ylab="Latitude ",
         col=alpha(animal_tags$Col,0.05), pch=19, cex=0.4, xaxt='n', yaxt='n')
    # Axis
    max_long <- max(animal_tags[, long_col_name])
    min_long <- min(animal_tags[, long_col_name])
    max_lat <- max(animal_tags[, lat_col_name])
    min_lat <- min(animal_tags[, lat_col_name])
    axis(side=1, at=seq(min_long, max_long, by=(max_long - min_long)/10), las=2,
         cex.axis=0.65, tck = 1, lty = 1, col = "gray")
    axis(side=2, at=seq(min_lat, max_lat, by=(max_lat - min_lat)/10), las=2,
         cex.axis=0.65, tck = 1, lty = 1, col = "gray")
    # Legend
    cuts <- gsub(",", " - ", cuts)
    cuts <- gsub("\\(", "[", cuts)
    #legend("topright", inset = c(- 0.15, 0), cuts, col=rbPal(bins), pch=16)
    legend("topright", cuts, col = rbPal(n_bins), pch = 16)
  }
}
