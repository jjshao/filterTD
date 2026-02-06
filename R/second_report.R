#' Wrapper function to generate secondary report that outlines how the data
#' will appear after certain processing with HPE and HPEm.
#'
#' @param sync_tag_data Table of data of sync tags
#' @param hpe_col_name String name of column that includes HPE (needs to be the same for all tag data tables)
#' @param hpem_col_name String name of column that includes HPEm
#' @param id_col_name String name of column that includes tag ID (needs to be the same across data tables)
#' @param time_col_name String, name of column that includes date/time data
#' @param date_format Character string giving a date-time format as used in base as.POSIX, default to "%Y/%m/%d %H:%M:%OS"
#' @param hpe_val Integer value for max HPE value user wants to display the graph to (with the max being the largest HPE value)
#' @param hpem_max Maximum HPEm value you are willing to have the data be
#' @param output_file String for output file name/location
#'
#'
#' @export
#'
#' @importFrom rmarkdown render


second_report <- function(sync_tag_data, hpe_col_name, hpem_col_name, id_col_name,
                          time_col_name, date_format = "%Y/%m/%d %H:%M:%OS",
                          hpe_val, hpem_max, output_file) {

  report_file <- system.file("reports", 'second.report.Rmd', package = "filterTelemetry")
  rmarkdown::render(report_file, params = list(sync_tag_data=sync_tag_data,
                                               hpe_col_name=hpe_col_name,
                                               hpem_col_name=hpem_col_name,
                                               id_col_name=id_col_name,
                                               time_col_name=time_col_name,
                                               date_format=date_format,
                                               hpem_max=hpem_max,
                                               hpe_val=hpe_val),
                    output_file = output_file)

}
