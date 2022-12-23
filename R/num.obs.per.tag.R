#' Get number of observations per tag when given all data or data is sorted
#' by species, sex, grouping, or a combination, can output graph. Grouping can
#' be used for a variety of different separations of data such as treatment.
#'
#' @param merged_tags Table of tag data
#' @param id_col_name String name of column that has tag IDs
#' @param graph Boolean, whether or not user wants to output a bar graph of number of observations per tag, default to FALSE
#' @param scatterplot Boolean, whether or not user wants to output a scatterplot of number of observations per size, default to FALSE
#' @param metadata Table of data that has metadata for tagged animals, must have column of ID tags
#' @param sp String, species name, default to FALSE
#' @param sp_col_name String, name of column that includes species name, default to FALSE, required if sp!=NULL
#' @param sex String, animal's sex, default to FALSE
#' @param sex_col_name String, name of column that includes animal's sex, default to FALSE, required if sex!=NULL
#' @param grouping String, grouping name, default to FALSE
#' @param grouping_col_name String, name of column that includes grouping, default to FALSE, required if grouping!=NULL
#' @param size_col_name String, name of column that includes size of animal, default to FALSE, required if scatterplot=TRUE
#' @return Vector of number of total number of observations and total number of tags (for given parameters) and graph if graph=TRUE
#' @examples
#' num_obs_per_tag_lob <- num.obs.per.tag(animal_merged, "Id", graph=TRUE,
#' sp="Homarus americanus", sp_col_name="Sp")
#'
#' @export

# ADD FUNCTIONALITY FOR TIME

num.obs.per.tag <- function(merged_tags, id_col_name, graph=FALSE, scatterplot=FALSE,
                            metadata=NULL, sp=NULL, sp_col_name=NULL, sex=NULL,
                            sex_col_name=NULL, grouping=NULL, grouping_col_name=NULL,
                            size_col_name=NULL) {
  if (missing(sp) & missing(sex) & missing(grouping)) {
    tags_subset <- merged_tags
  } else if (!missing(sp) & missing(sex) & missing(grouping)) {
    stopifnot(!missing(sp_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp) # subset of data this species
  } else if (missing(sp) & !missing(sex) & missing(grouping)) {
    stopifnot(!missing(sex_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, sex_col_name]==sex) # subset of data this sex
  } else if (missing(sp) & missing(sex) & !missing(grouping)) {
    stopifnot(!missing(grouping_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, grouping_col_name]==grouping) # subset of data in this group
  } else if (!missing(sp) & !missing(sex) & missing(grouping)) {
    stopifnot(!missing(sp_col_name) & !missing(sex_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp & merged_tags[, sex_col_name]==sex)
  } else if (!missing(sp) & missing(sex) & !missing(grouping)) {
    stopifnot(!missing(sp_col_name) & !missing(grouping_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp & merged_tags[, grouping_col_name]==grouping)
  } else if (missing(sp) & !missing(sex) & !missing(grouping)) {
    stopifnot(!missing(sex_col_name) & !missing(grouping_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, sex_col_name]==sex & merged_tags[, grouping_col_name]==grouping)
  } else {
    stopifnot(!missing(sp_col_name) & !missing(sex_col_name) & !missing(grouping_col_name))
    tags_subset <- subset(merged_tags, merged_tags[, sp_col_name]==sp & merged_tags[, sex_col_name]==sex & merged_tags[, grouping_col_name]) # subset of data this species and sex in this group
  }

  num_obs <- length(tags_subset[, id_col_name]) # total number of observations
  num_tags <- length(unique(tags_subset[, id_col_name])) # number of unique animals

  num <- table(tags_subset[, id_col_name]) # number of observations per unique animal
  maxy <- max(num) + 1000 # offset of maximum y for y-axis scale

  if (graph==TRUE & !missing(sex)) {
    barplot(num, main="Number of Observations per Tag",
            xlab="Tag", ylab="Number of Observations", ylim=c(0,maxy),
            cex.names=0.8, las=2, col="lightblue")
  } else if (graph==TRUE & missing(sex)) {
    stopifnot(!missing(metadata))

    # Add row for Id and merge to include sex column in num
    Id <- unique(tags_subset[, id_col_name])
    num <- rbind(num, Id)
    num <- t(num)
    num <- merge(num, metadata, by = "Id")

    # Colours to represent sex
    myColors <- ifelse(num$Sex=="F", "lightblue",
                       ifelse(num$Sex=="M", "lightslateblue", "white"))

    barplot(height=num$num, names=num$Id, main="Number of Observations per Tag",
            xlab="Tag", ylab="Number of Observations",
            ylim=c(0,maxy), cex.names=num_tags/100 - 0.1, las=2,
            col=myColors)
    legend("topright", legend = c("Female","Male") ,
           col = c("lightblue", "lightslateblue") , bty = "n",
           pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
  }

  if (scatterplot==TRUE) {
    stopifnot(!missing(metadata) & !missing(size_col_name))

    # Add row for Id and merge to include sex column in num
    Id <- unique(tags_subset[, id_col_name])
    num <- rbind(num, Id)
    num <- t(num)
    num <- merge(num, metadata, by = "Id")

    # Make colours "lightblue" and "lightslateblue" 50% transparent
    blue <- rgb(173, 216, 230, max = 255, alpha = 125)
    slate <- rgb(132, 112, 255, max = 255, alpha = 125)
    # Colours to represent sex
    myColors <- ifelse(num$Sex=="F", blue,
                       ifelse(num$Sex=="M", slate, "white"))

    plot(num$Size, num$num, main="Number of Observations per Size",
         xlab="Size", ylab="Number of Observations ", col=myColors, pch=19)
    legend("topright", legend = c("Female","Male") ,
           col = c("lightblue", "lightslateblue") , bty = "n",
           pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
  }

  return(c(num_obs, num_tags))
}
