#' Get number of observations per tag when given all data or data is sorted
#' by species, sex, grouping, or a combination, can output graph. Grouping can
#' be used for a variety of different separations of data such as treatment.
#'
#' @param merged_tags Table of tag data
#' @param id_col_name String name of column that has tag IDs
#' @param graph Boolean, whether or not user wants to output a bar graph of number of observations per tag, default to FALSE
#' @param scatterplot Boolean, whether or not user wants to output a scatterplot of number of observations per size, default to FALSE
#' @param metadata Table of data that has metadata for tagged animals, must have column of ID tags
#' @param grouping Vector of strings, grouping name, default to NULL
#' @param grouping_col_name Vector of strings, name of column that includes grouping, default to NULL, required if grouping!=NULL
#' @param continuous_grouping String, name of grouping of continuous variable, default to NULL
#' @param continuous_col_name String, name of column that includes continuous grouping, default to NULL, required if grouping!=NULL
#' @param size_col_name String, name of column that includes size of animal, default to FALSE, required if scatterplot=TRUE
#' @param start_date String, start date in format YYYY-MM-DD, default to NULL
#' @param end_date String, end date in format YYYY-MM-DD, default to NULL
#' @param time_interval String, interval of time to separate data into, either "week" or "month", default to NULL
#' @param time_col_name String, name of column that includes date/time data, default to NULL
#' @return Vector of number of total number of observations and total number of tags (for given parameters) and graph if graph=TRUE or scatterplot=TRUE,
#' or returns integers of number of observations per week/month if date range given
#' @examples
#' # Load data
#' sample_file <- system.file("extdata", "dummy_animals.csv", package = "filterTelemetry")
#' animal_tag_data <- read.table(file = sample_file, header=TRUE, sep=",")
#' sample_metadata <- system.file("extdata", "dummy_metadata.csv", package = "filterTelemetry")
#' metadata <- read.table(file = sample_metadata, header=TRUE, sep=",")
#' animal_merged <- merge(animal_tag_data, metadata, by = "Id")
#'
#' # Example to make a scatterplot
#' num_obs_per_tag_size <- num_obs_per_tag(animal_merged, "Id", graph=TRUE,
#'                                         grouping=c("Cancer irroratus", "M"),
#'                                         grouping_col_name=c("Sp", "Sex"),
#'                                         continuous_grouping=c("Size"),
#'                                         continuous_col_name=c("Size"),
#'                                         metadata=metadata, scatterplot=TRUE)
#'
#' @export
#'
#' @importFrom graphics barplot


num_obs_per_tag <- function(merged_tags, id_col_name, graph=FALSE, scatterplot=FALSE,
                            metadata=NULL, grouping=NULL, grouping_col_name=NULL,
                            continuous_grouping=NULL, continuous_col_name=NULL,
                            size_col_name=NULL, start_date=NULL, end_date=NULL,
                            time_interval=NULL, time_col_name=NULL) {

  # Subset merged_tags by grouping
  if (missing(grouping)) {
    tags_subset <- merged_tags
  } else {
    if (length(grouping) > 1) {
      stopifnot(length(grouping) == length(grouping_col_name))
      tags_subset <- merged_tags
      count <- 1
      for(x in grouping) {
        tags_subset <- subset(tags_subset, tags_subset[, grouping_col_name[count]]==x)
        count <- count + 1
      }
    } else if (length(grouping) == 1) {
      grouping <- grouping[1]
      grouping_col_name <- grouping_col_name[1]
      tags_subset <- subset(merged_tags, merged_tags[, grouping_col_name]==grouping)
    }
  }

  # Subset tags_subset by dates if provided
  if (!missing(start_date) | !missing(end_date) | !missing(time_interval)) {
    # Check to see that we have all the information needed to segment data based on time interval
    stopifnot(!missing(start_date) & !missing(end_date) & !missing(time_interval) & !missing(time_col_name))
    stopifnot(time_interval=="week" | time_interval=="month")

    # Ensure that tag data date column are characters
    tags_subset[, time_col_name] <- as.character(tags_subset[, time_col_name])
    # Split into date and time
    tags_subset[, "date"] <- sapply(sapply(tags_subset[, time_col_name],function(x)strsplit(x," ")),function(x)x[1])
    tags_subset[, "time"] <- sapply(sapply(tags_subset[, time_col_name],function(x)strsplit(x," ")),function(x)x[2])
    # Change to be in R Date
    tags_subset[, "date"] <- as.Date(tags_subset[, "date"],format='%Y-%m-%d')
    # Subset of data within the time interval
    tags_subset <- subset(tags_subset,
                          tags_subset[, "date"] < as.Date(end_date) | tags_subset[, "date"] > as.Date(start_date))

    if (time_interval=="week") {
      # Add column to show week number
      tags_subset[,"week_num"] <- strftime(tags_subset[, "date"], format = "%V")
      # Count number of times these occur
      num_obs_per_time <- table(tags_subset[, "week_num"])
      return(num_obs_per_time)
    } else {
      # Add column for month
      tags_subset[,"month_num"] <- format(as.Date(tags_subset[, "date"], format="%Y-%m-%d"),"%m")
      # Count number of times these occur
      num_obs_per_time <- table(tags_subset[, "month_num"])
      return(num_obs_per_time)
    }
  }

  num_obs <- length(tags_subset[, id_col_name]) # total number of observations
  num_tags <- length(unique(tags_subset[, id_col_name])) # number of unique animals

  num <- table(tags_subset[, id_col_name]) # number of observations per unique animal
  maxy <- max(num) + 1000 # offset of maximum y for y-axis scale

  if (graph==TRUE & !missing(grouping)) {
    barplot(num, main=paste("Number of Observations per Tag for", grouping, sep=" "),
            xlab="Tag", ylab="Number of Observations", ylim=c(0,maxy),
            cex.names=0.8, las=2, col="lightblue")
  }


  if (scatterplot==TRUE) {
    stopifnot(!missing(metadata) & !missing(continuous_grouping) & !missing(continuous_col_name))

    # Add row for Id and merge to include sex column in num
    Id <- unique(tags_subset[, id_col_name])
    num <- rbind(num, Id)
    num <- t(num)
    num <- merge(num, metadata, by = "Id")

    plot(num[, continuous_col_name], num$num, main="Number of Observations per Size",
         xlab=continuous_grouping, ylab="Number of Observations ", pch=19)
  }

  return(c(num_obs, num_tags))
}

