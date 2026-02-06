#' Finds a list of which values in a column occur more than a specified number of times in a dataset
#'
#' @name filter.numdet
#' @param column.name Name of column to count
#' @param num.det Minimum number of times occurred
#' @return A list of numerics contained in the column that occur more than num.det times
#' @examples
#' # Load data
#' animal_tag_data <- read.table(file = './extdata/dummy_animals.csv', header=TRUE, sep=",")
#' # Example
#' anitags_200 <- filter.numdet(animal_tag_data$Id, 200)
#' head(anitags_200)
#'
#' @export

filter.numdet <- function(column.name, num.det) {
  foo <- as.numeric(names(which(table(column.name) > num.det)))
  return(foo)
}
