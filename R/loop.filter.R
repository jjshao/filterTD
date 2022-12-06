# Function to..
# data.from: the df of data to filter from
# loop.list: list of tags to loop through
# column.name: string name of column that loop.list is a list of
#
# Outputs a list of data

# Make this an internal function?


library(dplyr)
# NOTE: Can do without a for loop if use dplyr

loop.filter <- function(data.from, loop.list, column.name) {
  data.from %>%
    filter(column.name %in% loop.list)
  return(data.from)
}

