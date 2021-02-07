#' A data frame of Monday.com boards
#'
#' Get a list of all boards the aunthenticated user has access to
#'
#' @export
#' @examples
#' monday_list_boards()
monday_list_boards <- function(){
  data <- monday_query("{boards(limit: 10000){name, id}}")

  # Check if there are any boards
  if(length(data$data$boards) == 0){

  }

  # Extract Board data
  data <- data$data$boards

  # Convert into a data frame
  df <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE), stringsAsFactors = FALSE)

  # Get the correct colnames
  colnames(df) <- names(data[[1]])

  return(df)
}
