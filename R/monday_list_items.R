#' Get items from Monday.com
#'
#' A data frame of Monday.com items/pulses
#'
#' @param board_id The id of a Monday.com board (optional)
#' @param limit Choose a maximum number of items to get (optional)
#' @param custom_item_query Provide your own item query (optional)
#'
#' @export
#' @examples
#' monday_list_items(board_id = 1234, limit = 10, custom_item_query="{items{name}}")
monday_list_items <- function(board_id = NULL, limit = NULL, custom_item_query = NULL){
  if(is.null(custom_item_query)){
    item_string <- "{id, name, board{id, name}}"
  } else {
    item_string <- custom_item_query
  }

  # Run if no board id is provided
  if(is.null(board_id)){

    # Add limit if provided
    if(!is.null(limit)){
      items <- monday_query(paste0(
        "{items(newest_first: true, limit: ", limit, ")", item_string, "}"
      ))$data$items
    } else {

      items <- NULL
      items_left <- TRUE
      page <- 1

      while(items_left){
        item_fetch <- monday_query(paste0(
          "{items(newest_first: true, limit: 25, page: ", page, ")", item_string, "}"
        ))$data$items

        items <- c(items, item_fetch)
        page <- page+1

        if(length(item_fetch) < 25){
          items_left <- FALSE
        }
      }
    }
  } else { # Run if board id is provided
    boards <- monday_query(paste0(
      "{boards(ids: ", board_id, "){name}}"

    ))$data$boards

    # Check if the provided id represents a board
    if(length(boards) == 0){
      stop("No board found with that id!", call. = FALSE)
    }

    # Get items from board
    if(!is.null(limit)){
      items <- monday_query(paste0(
        "{boards(ids: ", board_id, "){items(newest_first: true, limit: ", limit, ")", item_string, "}}"
      ))$data$boards[[1]]$items
    } else {

      items <- NULL
      items_left <- TRUE
      page <- 1

      while(items_left){
        item_fetch <- monday_query(paste0(
          "{boards(ids: ", board_id, "){items(newest_first: true, limit: 25, page: ", page, ")", item_string, "}}"
        ))$data$boards[[1]]$items

        items <- c(items, item_fetch)
        page <- page+1

        if(length(item_fetch) < 25){
          items_left <- FALSE
        }
      }
    }
  }

  item_data <- as.data.frame(
    matrix(
      unlist(items),
      nrow = length(items),
      byrow = TRUE
    ),
    stringsAsFactors = TRUE
  )

  colnames(item_data) <- gsub("\\.", "_", names(unlist(items)[1:ncol(item_data)]))

  return(item_data)
}
