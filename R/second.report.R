#' Wrapper function to generate secondary report that outlines how the data
#' will appear after certain processing with HPE and data loss percentages.
#'
#' @name second.report
#' @param data
#' @examples
#'
#'
#' @export
#'

second.report <- function() {
  rmarkdown::render('second.report.Rmd')

}
