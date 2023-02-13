#' Wrapper function to generate secondary report that outlines how the data
#' will appear after certain processing with HPE and data loss percentages.
#'
#' @name second.report
#'
#'
#' @export
#'

second.report <- function(sync_tag_data, hpe_col_name, hpem_col_name, id_col_name,
                          time_col_name, percentage=NULL, hpem_max=NULL) {

  if(missing(percentage) & !missing(hpem_max)) {
    rmarkdown::render('second.report.hpem.Rmd', params = list(sync_tag_data=sync_tag_data,
                                                         hpe_col_name=hpe_col_name,
                                                         hpem_col_name=hpem_col_name,
                                                         id_col_name=id_col_name,
                                                         time_col_name=time_col_name,
                                                         hpem_max=hpem_max))
  } else if(!missing(percentage) & missing(hpem_max)) {
    rmarkdown::render('second.report.Rmd', params = list(sync_tag_data=sync_tag_data,
                                                         hpe_col_name=hpe_col_name,
                                                         hpem_col_name=hpem_col_name,
                                                         id_col_name=id_col_name,
                                                         time_col_name=time_col_name,
                                                         percentage=percentage))
  }

}
