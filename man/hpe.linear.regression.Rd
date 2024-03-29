% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpe.linear.regression.R
\name{hpe.linear.regression}
\alias{hpe.linear.regression}
\title{Calculate linear regression given HPE level or percentage of data.
Accepts only one of hpem_max or percentage.}
\arguments{
\item{sync_tags}{Table of data of sync tags}

\item{hpe_col_name}{String name of column in sync_tags that has HPE values}

\item{hpem_col_name}{String name of column in sync_tags that has HPEm values}

\item{hpe_val}{Integer value for max HPE value user wants to display the graph to (with the max being the largest HPE value)}
}
\value{
Output of plot HPE versus measured error to the median point with white circles with black outline and red x representing the two dimensional root mean square error of x and y components of error within an HPE bin
}
\description{
Calculate linear regression given HPE level or percentage of data.
Accepts only one of hpem_max or percentage.
}
\examples{
# Load dataset
dummy_sync <- read.table(file = './extdata/dummy_sync.csv', header=TRUE, sep=",")
# Example
sync_linreg <- hpe.linear.regression(dummy_sync, "HPE", "HPEm", percentage=0.1)

}
