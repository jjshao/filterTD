#' Calculate linear regression given HPE level or percentage of data.
#' Accepts only one of hpem_max or percentage.
#'
#' @name hpe.linear.regression
#' @param sync_tags Table of data of sync tags
#' @param hpe_col_name String name of column in sync_tags that has HPE values
#' @param hpem_col_name String name of column in sync_tags that has HPEm values
#' @param percentage Percentage of data you are willing to lose, default to NULL
#' @param hpem_max Maximum HPEm value you are willing to have the data be, default to NULL
#' @return Vector of three values (p-value, r-squared, and percentage of removed data as a decimal).
#'
#' @examples
#' # Load dataset
#' dummy_sync <- read.table(file = './extdata/dummy_sync.csv', header=TRUE, sep=",")
#' # Example
#' sync_linreg <- hpe.linear.regression(dummy_sync, "HPE", "HPEm", percentage=0.1)
#'
#' @export


require(ggplot2)

hpe.linear.regression <- function(sync_tags, hpe_col_name, hpem_col_name,
                                  percentage=NULL, hpem_max=NULL) {
  if(!missing(percentage) & !missing(hpem_max)) {
    print("Give only one percentage or hpem_max")
    stop()
  }

  if(missing(percentage) & !missing(hpem_max)) {
    # hpem <= 2 means lose all data points that has hpem > 2
    #sync_tags$prob <- sync_tags[, hpem_col_name]
    sync_prob <- subset(sync_tags, sync_tags[, hpem_col_name]<=hpem_max)
  } else if(!missing(percentage) & missing(hpem_max)) {
    # percentage 0.1 means willing to lose 10% of data
    quantile_data <- quantile(sync_tags[, hpe_col_name], probs = percentage)
    # Segment sync_tags to keep only rows where the HPE is less than or equal to
    #the accepted HPE for that quantile
    sync_prob <- sync_tags[sync_tags[, hpe_col_name] <= quantile_data, ]
  }

  # percent remaining
  perc_remain <- nrow(sync_prob)/nrow(sync_tags)

  # Linear regression of HPEm and HPE
  mod <- lm(sync_prob[, hpem_col_name]~sync_prob[, hpe_col_name])

  print(ggplot(sync_prob, aes(sync_prob[, hpe_col_name], y=sync_prob[, hpem_col_name]))+
    geom_point() + theme_bw() +
    geom_smooth(method="lm", formula=y~x) +
    annotate(label = sprintf("y = %.3f x\nR = %.3f", coef(mod), summary(mod)$r.squared),
             geom = "text", x = (min(sync_prob[, hpe_col_name])*1.01), y = (max(sync_prob[, hpem_col_name])*0.99), size = 4,
             colour = "red", check_overlap = TRUE) +
    labs(y = "HPEm", x = "HPE", title = "Linear Regression of HPE and HPEm"))

  return(c(summary(mod)$coefficients[,4], summary(mod)$r.squared, perc_remain))
}
