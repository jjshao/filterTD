# function to get number of observations per tag

num.obs.per.tag <- function(merged_tags_id, merged_tags, graph=FALSE, sp=NULL, sex=NULL) {
  if (missing(sp) & missing(sex)) {
    num <- table(merged_tags_id) # number of observations per unique animal
    print(typeof(num))
    maxy <- max(num) + 1000
    print(maxy)

    num_obs <- length(merged_tags_id) # total number of observations
    num_tags <- length(unique(merged_tags_id)) # number of unique animals
  } else if (!missing(sp) & missing(sex)) {
    print("working")
    tags_subset <- merged_tags %>% filter(Sp == "Cancer irroratus")

  } else if (missing(sp) & !missing(sex)) {
    print("working")
  } else {
    print("working")
  }

  if (graph==TRUE) {
    barplot(num, main="Number of Observations per Tag",
            xlab="Tag", ylim=c(0,maxy), cex.names=0.5, las=2)
  }
  return(c(num_obs, num_tags))
}
