#' A data frame of columns
#'
#' Get the columns of a Monday.com board
#'
#' @param board_id The id of a Monday.com board
#'
#' @export
#' @examples
#' monday_list_columns(board_id = 1234)
monday_list_columns <- function(board_id){
  boards <- monday_query(paste0(
    "{boards(ids: ", board_id, "){id, name, columns{id, title, type}}}"

  ))$data$boards

  # Check if the provided id represents a board
  if(length(boards) == 0){
    stop("No board found with that id!", call. = FALSE)
  }

  # Get board data
  board_name <- boards[[1]]$name
  board_id <- boards[[1]]$id

  # Extract all column data
  columns <- boards[[1]]$columns

  columns <- as.data.frame(
    matrix(
      unlist(columns),
      nrow = length(columns),
      dimnames = list(NULL,names(columns[[1]])),
      byrow = TRUE
    ),
    stringsAsFactors = TRUE
  )

  columns <- cbind(
    columns,
    data.frame(
      board_id,
      board_name,
      stringsAsFactors = FALSE
    )
  )

  return(columns)
}
