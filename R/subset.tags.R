#' Subset tags from files
#'
#' @name subset.tags
#' @param tags Table of all tags or just animal tags (if there is a sync tag file)
#' @param sync_tags Table of all sync tags, default to NULL
#' @param id_col String that is what the column for id/transmitter is in all tables
#' @param sync_tags.vec Vector of strings, tag numbers for all sync tags
#' @param ref_tags.vec Vector of strings, tag numbers for all reference tags
#' @param ani_tags.vec Vector of strings, tag numbers for all animal tags
#' @return A list of three dataframes that give animal tag data, sync tag data, and reference tag data
#' @examples
#' # Load data
#' all_tags <- read.table(file = './extdata/dummy_animals.csv', header=TRUE, sep=",")
#' other_tags <- read.table(file = './extdata/dummy_sync.csv', header=TRUE, sep=",")
#' ani_tags <- unique(all_tags$Id)
#' non_ref <- subset(other_tags, Id!=61034)
#' sync_tags <- unique(non_ref$Id)
#' ref_tags <- c(61034)
#' # Example
#' test_ <- subset.tags(tags = all_tags, id_col = "Id",
#'                      sync_tags.vec = sync_tags, ref_tags.vec = ref_tags,
#'                      ani_tags.vec = ani_tags)
#'
#' @export

require(dplyr)

subset.tags <- function(tags, sync_tags=NULL, id_col, sync_tags.vec, ref_tags.vec, ani_tags.vec) {
  if(missing(sync_tags)) {
    animal_tag_data <- dplyr::filter(tags, tags[,id_col] %in% ani_tags.vec)
    sync_tag_data <- dplyr::filter(tags, tags[,id_col] %in% sync_tags.vec)
    ref_tag_data <- dplyr::filter(tags, tags[,id_col] %in% ref_tags.vec)
  } else {
    animal_tag_data <- dplyr::filter(tags, tags[,id_col] %in% ani_tags.vec)
    sync_tag_data <-  dplyr::filter(sync_tags, sync_tags[,id_col] %in% sync_tags.vec)
    ref_tag_data <- dplyr::filter(sync_tags, sync_tags[,id_col] %in% ref_tags.vec)
  }
  return(list(animal_tag_data, sync_tag_data, ref_tag_data))
}



