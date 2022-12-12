# function to get number of observations per tag

num.obs.per.tag <- function(merged_tags, id_col_name, graph=FALSE, sp=NULL,
                            sp_col_name=NULL, sex=NULL, sex_col_name=NULL) {
  if (missing(sp) & missing(sex)) {
    tags_subset <- merged_tags
  } else if (!missing(sp) & missing(sex)) {
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp) # subset of data this species
  } else if (missing(sp) & !missing(sex)) {
    tags_subset <- subset(merged_tags, merged_tags[, sex_col_name]==sex)
  } else {
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp & merged_tags[, sex_col_name]==sex)
  }

  print(head(tags_subset))
  num <- table(merged_tags[, id_col_name]) # number of observations per unique animal
  print(typeof(num))
  print(num)
  maxy <- max(num) + 1000

  num_obs <- length(merged_tags[, id_col_name]) # total number of observations
  num_tags <- length(unique(merged_tags[, id_col_name])) # number of unique animals

  if (graph==TRUE) {
    barplot(num, main="Number of Observations per Tag",
            xlab="Tag", ylim=c(0,maxy), cex.names=0.5, las=2)
  }
  return(c(num_obs, num_tags))
}
