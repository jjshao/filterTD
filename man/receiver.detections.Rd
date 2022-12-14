% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/receiver.detections.R
\name{receiver.detections}
\alias{receiver.detections}
\title{Function that outputs a scatterplot of all observations of sync tags
with longitude as the x-axis and latitude as the y-axis and the colour of
each point represents HPE.}
\arguments{
\item{sync_tags}{Table of data of sync tags}

\item{sync_hpe_col_name}{String name of column in sync_tags that has HPE values}

\item{animal_tags}{Table of data of animal tags}

\item{ani_hpe_col_name}{String name of column in animal_tags that has HPE values}

\item{lat_col_name}{String name of column in animal_tags that has latitude}

\item{long_col_name}{String name of column in animal_tags that has longitude}

\item{num_bins}{Integer number of bins that user wants HPE to be split into}
}
\description{
Function that outputs a scatterplot of all observations of sync tags
with longitude as the x-axis and latitude as the y-axis and the colour of
each point represents HPE.
}
\examples{
receiver.detections(sync_tags=sync_tag_data, sync_hpe_col_name="HPE",
                    animal_tags=animal_tag_data, ani_hpe_col_name="HPE",
                    num_bins=7, lat_col_name="Latitude", long_col_name="Longitude")

}
