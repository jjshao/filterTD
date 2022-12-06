# Function to subset tags from one file
#
# tags: file with all tags or animal tags (if there is a sync tag file)
# sync_tags: file with sync tags, can be null
# id_col: string that is what the column for id/transmitter is called
# sync_tags.vec: vector of strings, tag numbers for all sync tags
# ref_tags.vec: vector of strings, tag numbers for all reference tags
# ani_tags.vec: vector of strings, tag numbers for all animal tags
#
# Outputs a list of three df giving animal tag data, sync tag data, and reference tag data

subset.tags <- function(tags, sync_tags=NULL, id_col, sync_tags.vec, ref_tags.vec, ani_tags.vec) {
  if(missing(sync_tags)) {
    animal_tag_data <- loop.filter(tags, ani_tags.vec, id_col)
    sync_tag_data <- loop.filter(tags, sync_tags.vec, id_col)
    ref_tag_data <- loop.filter(tags, ref_tags.vec, id_col)
  } else {
    animal_tag_data <- loop.filter(tags, ani_tags.vec, id_col)
    sync_tag_data <- loop.filter(sync_tags, sync_tags.vec, id_col)
    ref_tag_data <- loop.filter(sync_tags, ref_tags.vec, id_col)
  }
  return(list(animal_tag_data, sync_tag_data, ref_tag_data))
}



