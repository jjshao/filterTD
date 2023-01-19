#' Wrapper function to generate preliminary report.
#'
#' @name prelim.report
#' @param Tags File pathway to table with tags, needs to be comma delineated
#' @param SyncTags File pathway to table with sync tags (if separate from file with tags), needs to be comma delineated
#' @param Metadata File pathway to table of metadata for all animal tags, needs to be semicolon delineated
#' @param ani_tags Vector of all animal tags (e.g. c(15820, 14325, 86402))
#' @param sync_tags Vector of all sync tags (e.g. c(65106))
#' @param ref_tags Vector of all reference tags (e.g. c(65321, 65322))
#' @param id_col_name String name of column that includes tag ID (needs to be the same across data tables)
#' @param vec_species Vector of string species names (e.g. c("Cancer irroratus","Homarus americanus"))
#' @param hpe_col_name String name of column that includes HPE (needs to be the same for all tag data tables)
#' @param bins Vector of doubles for HPE separations to make bins
#' @param num_bins Double number of bins that user wants HPE separated into
#' @param lat_col_name String name of column that includes latitude
#' @param long_col_name String name of column that includes longitude
#' @param sp_col_name String name of column that includes species name in metadata file
#' @examples
#'
#'
#' @export
#'

prelim.report <- function(Tags, SyncTags=NULL, Metadata, ani_tags, sync_tags,
                          ref_tags, id_col_name, vec_species, hpe_col_name, bins=NULL,
                          num_bins, lat_col_name, long_col_name, sp_col_name) {

  if (!missing(SyncTags)) {
    # Reading data files
    all_tags <- read.table(file = Tags, header=TRUE, sep=",")
    other_tags <- read.table(file = SyncTags, header=TRUE, sep=",")

    # Subsetting data into usable lists
    tags_subset <- subset.tags(tags = all_tags,
                               sync_tags = other_tags,
                               id_col = id_col_name,
                               sync_tags.vec = sync_tags,
                               ref_tags.vec = ref_tags,
                               ani_tags.vec = ani_tags)
  } else {
    # Reading data files
    all_tags <- read.table(file = Tags, header=TRUE, sep=",")

    # Subsetting data into usable lists
    tags_subset <- subset.tags(tags = all_tags,
                               id_col = id_col_name,
                               sync_tags.vec = sync_tags,
                               ref_tags.vec = ref_tags,
                               ani_tags.vec = ani_tags)

  }
  rmarkdown::render('prelim.report.Rmd', params = list(tags=tags_subset,
                                                       id_col_name = id_col_name,
                                                       sync_tags.vec = sync_tags,
                                                       ref_tags.vec = ref_tags,
                                                       ani_tags.vec = ani_tags,
                                                       vec_species = vec_species,
                                                       hpe_col_name = hpe_col_name,
                                                       hpem_col_name = hpem_col_name,
                                                       bins = bins,
                                                       num_bins = num_bins,
                                                       lat_col_name = lat_col_name,
                                                       long_col_name = long_col_name,
                                                       sp_col_name = sp_col_name))

}
