#' Subset tags from one a table
#'
#' @param data.from Table that user wants to sort through
#' @param loop.vec Vector of values that user wants to find
#' @param column.name Name of column that contains the values within loop.vec
#' @return A dataframe with all rows from original table whose specified column includes a value in loop.vec
#' @examples
#' data <- loop.filter(tags, tagnames.vec, id_col)
#'

loop.filter <- function(data.from, loop.vec, column.name) {
  data.from %>%
    filter(column.name %in% loop.vec)
  return(data.from)
}

