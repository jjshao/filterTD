# Function to subset tags from one file
#
# tags: file with all tags or animal tags (if there is a sync tag file)
# sync_tags: file with sync tags, can be null
# id_col: string that is what the column for id/transmitter is called
# list.sync_tags: list of strings, tag numbers for all sync tags
# list.ref_tags: list of strings, tag numbers for all reference tags
# list.ani_tags: list of strings, tag numbers for all animal tags
#
# Outputs a list of three lists giving animal tag data, sync tag data, and reference tag data

subset.tags <- function(tags, sync_tags=NULL, id_col, list.sync_tags, list.ref_tags, list.ani_tags) {
  if(missing(sync_tags)) {
    animal_tag_data <- loop.filter(tags, list.ani_tags, id_col)
    sync_tag_data <- loop.filter(tags, list.sync_tags, id_col)
    ref_tag_data <- loop.filter(tags, list.ref_tags, id_col)
  } else {
    ref_tag_data <- loop.filter(sync_tags, list.ref_tags, id_col)
  }
  return(list(animal_tag_data, sync_tag_data, ref_tag_data))
}



