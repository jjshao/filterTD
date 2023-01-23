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
  rmarkdown::render('second.report.Rmd', params = list(hpe=hpe))

}

# Test for willing to lose: 10% and 50% of data
# Test for HPEm: error <2m and <30m
