# Function that takes a dataframe, the name of one df column loop, and a list
# that presumably has items that exist in that df column, loops through the list
# and selects all the rows with that item and returns a df of all the selected
# rows
#
# data.from: the df of data to filter from
# loop.list: list of tags to loop through
# column.name: string name of column that loop.list is a list of
#
# Outputs a list of data


# Make this an internal function?


loop.filter <- function(data.from, loop.list, column.name) {
  data <- subset(data.from, column.name == loop.list[1])
  loop.list <- loop.list[-1]
  for (x in loop.list) {
    foo <- subset(data.from, column.name == x)
    data <- rbind(data, foo)
  }
  return(data)
}

