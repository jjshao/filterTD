# Function to give list GREATER THAN number detections
# data.from: the df of data to filter from
# column.name: string name of column that loop.list is a list of
# num_det: integer for lowest number of detections
#
# Outputs a list of data

filter.numdet <- function(column.name, num_det) {
  foo <- as.numeric(names(which(table(column.name) > num_det)))
  return(foo)
}
