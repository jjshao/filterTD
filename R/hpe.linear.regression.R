#' Calculate linear regression given HPE level or percentage of data.
#' Accepts only one of hpem_max or percentage.
#'
#' @name hpe.linear.regression
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param hpe_val Integer value for max HPE value user wants to display the graph to (with the max being the largest HPE value)
#' @param id_col_name String name of column in sync_tags that has tag ID
#' @param x_col_name String name of column in sync_tags that has x coordinates
#' @param y_col_name String name of column in sync_tags that has y coordinates
#' @return Returns the linear regression between RMS2d errors and HPE and output of plot HPE versus measured error to the median point with white circles with black outline and red x representing the two dimensional root mean square error of x and y components of error within an HPE bin
#'
#' @examples
#' # Load dataset
#' dummy_sync <- read.table(file = './extdata/dummy_sync.csv', header=TRUE, sep=",")
#' # Example
#' sync_linreg <- hpe.linear.regression(dummy_sync, "HPE", "HPEm", percentage=0.1)
#'
#' @export


require(ggplot2)
require(dplyr)

hpe.linear.regression <- function(sync_tags, hpe_col_name, hpem_col_name,
                                  hpe_val, id_col_name, x_col_name, y_col_name) {
  # median positions for the station/sync tags - on an X/Y coordinate system instead of lat/long
  StatTrans_medPos <- sync_tags %>%
    filter(HPE < 250) %>%
    group_by(!!sym(id_col_name)) %>%
    summarize(med.x = median(!!sym(x_col_name), na.rm = TRUE),
              med.y = median(!!sym(y_col_name), na.rm = TRUE))

  # Create a new column with error.m as a column so it's easier to call/use in the future.
  # This represents the error in m (distance) associated with each position fix (as a distance from
  # the median position calculated for that stationary tag)
  sync_tags <- sync_tags %>%
    # filter(HPE < 250) %>%
    left_join(StatTrans_medPos) %>%
    mutate(error.m = sqrt((!!sym(x_col_name) - med.x)^2 + (!!sym(y_col_name) - med.y)^2))

  # First have to create a bin column to bin data by HPE
  # create increments to bin by
  breaks = seq(from = 0, to = max(sync_tags[[hpe_col_name]]), by=1)
  # create a column with bins
  sync_tags$bin <- cut(sync_tags[[hpe_col_name]], breaks)

  binMean <- sync_tags %>%
    mutate(Xe = sqrt((!!sym(x_col_name) - med.x)^2)) %>%
    mutate(Ye = sqrt((!!sym(y_col_name) - med.y)^2)) %>%
    group_by(!!sym(id_col_name), bin) %>%
    summarize(bin_mean = mean(error.m, na.rm = TRUE), bin_num = length(error.m), Xe_Sd = sd(Xe), Ye_Sd = sd(Ye),
              RMS2d = 2*sqrt((Xe_Sd)^2 + (Ye_Sd)^2), avgHPE = mean(!!sym(hpe_col_name)))

  smallBin <- binMean[which(binMean$bin_num > 10),]
  smallBin <- smallBin[which(smallBin$avgHPE < hpe_val),]

  # Simple linear regression between the RMS2d errors and the HPE
  res3 <- lm(smallBin$RMS2d ~ smallBin$avgHPE)


  # Plot 2a: The HPE versus measured error to the median point is shown for each estimated position
  # during the tests for a fixed test (black dots). The white circles with black outline and red x represent
  # the two dimensional root mean square error of x and y components of error within an HPE bin of one.
  # 95 percent of tag detections have an error less than this point within each bin. The line running between
  # these points represents the two dimensional root means square error (2DRMS) and the equation and t
  # for this line are shown in the top left corner.
  # Graph of HPE to error relationship
  print(sync_tags %>% ggplot(aes(!!sym(hpe_col_name), error.m)) +
    geom_point() +
    scale_y_continuous(limits = c(0, hpe_val)) +
    scale_x_continuous(limits = c(0, hpe_val)) +
    theme_bw() +
    geom_point(data = smallBin, aes(x = avgHPE, y = RMS2d), shape = 21, size = 5,
               col = 'black', bg = 'white', alpha = 1) +
    geom_point(data=smallBin, aes(x = avgHPE, y = RMS2d), shape= 4, size = 3, color = 'red') +
    theme(legend.position = "none") +
    geom_abline(data=res3, aes(intercept = res3$coefficients[1], slope = res3$coefficients[2]),
                col='white', size=1) +
    geom_abline(data=res3, aes(intercept = res3$coefficients[1], slope=res3$coefficients[2]),
                col = 'black', linetype='dashed', linewidth = 1) +
    annotate("text", x = -Inf, y = Inf,
             hjust = -0.05, vjust = 1.1,
             label = lm.eqn(res3), size= 6, parse=TRUE, color = 'red'))

  return(res3)
}
