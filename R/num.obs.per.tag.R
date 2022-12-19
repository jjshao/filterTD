#' Get number of observations per tag when given all data or data is sorted
#' by species or sex, can output graph
#'
#' @param merged_tags Table of data (requires metadata if using sp or sex)
#' @param id_col_name String name of column that has tag IDs
#' @param graph Boolean, whether or not user wants an outputted bar graph of number of observations per tag, default to FALSE
#' @param sp String, species name, default to FALSE
#' @param sp_col_name String, name of column that includes species name, default to FALSE, required if sp!=NULL
#' @param sex String, animal's sex, default to FALSE
#' @param sex_col_name String, name of column that includes animal's sex, default to FALSE, required if sex!=NULL
#' @return Vector of number of total number of observations and total number of tags (for given parameters) and graph if graph=TRUE
#' @examples
#' num_obs_per_tag_lob <- num.obs.per.tag(animal_merged, "Id", graph=TRUE,
#' sp="Homarus americanus", sp_col_name="Sp")
#'
#' @export

num.obs.per.tag <- function(merged_tags, id_col_name, graph=FALSE, sp=NULL,
                            sp_col_name=NULL, sex=NULL, sex_col_name=NULL, metadata=NULL) {
  if (missing(sp) & missing(sex)) {
    tags_subset <- merged_tags
  } else if (!missing(sp) & missing(sex)) {
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp) # subset of data this species
  } else if (missing(sp) & !missing(sex)) {
    tags_subset <- subset(merged_tags, merged_tags[, sex_col_name]==sex)
  } else {
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp & merged_tags[, sex_col_name]==sex)
  }

  num <- table(tags_subset[, id_col_name]) # number of observations per unique animal
  maxy <- max(num) + 1000 # offset of maximum y for y-axis scale

  if (graph==TRUE & !missing(sex)) {
    barplot(num, main="Number of Observations per Tag",
            xlab="Tag", ylab="Number of Observations", ylim=c(0,maxy),
            cex.names=0.5, las=2, col="lightblue")
  } else if (graph==TRUE & missing(sex)) {
    # Add row for Id and merge to include Sex column in num
    Id <- unique(tags_subset[, id_col_name])
    num <- rbind(num, Id)
    num <- t(num)
    num <- merge(num, metadata, by = "Id")

    # Colours to represent sex
    myColors <- ifelse(num$Sex=="F", "lightblue",
                       ifelse(num$Sex=="M", "lightslateblue", "white"))

    barplot(height=num$num, names=num$Id, main="Number of Observations per Tag",
            xlab="Tag", ylab="Number of Observations",
            ylim=c(0,maxy), cex.names=0.7, las=2,
            col=myColors)
    legend("topleft", legend = c("Female","Male") ,
           col = c("lightblue", "lightslateblue") , bty = "n",
           pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
  }

  num_obs <- length(tags_subset[, id_col_name]) # total number of observations
  num_tags <- length(unique(tags_subset[, id_col_name])) # number of unique animals

  return(c(num_obs, num_tags))
}
