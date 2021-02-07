#' Get column values
#'
#' Get column values for Monday.com items
#'
#' @param item_id The id of a Monday.com item (optional)
#' @param board_id The id of a Monday.com board (optional)
#' @param column_id The id of a Monday.com board column (optional)
#' @param column_title The title of a Monday.com board column (optional)
#' @param column_type The type of a Monday.com board column (optional)
#' @param raw Wether or not to return raw or structured data (defaults to FALSE)
#'
#'
#' @export
#' @examples
#' monday_column_values(item_id = 123, board_id = 1234, column_title="Status")
monday_column_values <- function(item_id = NULL, board_id = NULL, column_id = NULL, column_title = NULL, column_type = NULL, raw = FALSE){
  if(all(is.null(c(item_id, board_id)))){
    stop("Must provide either board_id or item_id", call. = FALSE)
  }

  if(!is.null(board_id)){
    for(i in 1:(length(board_id))){
      if(!board_id[i] %in% monday_list_boards()$id){
        stop(paste0("The board id '", board_id[i], "' does not exist in your account"), call. = FALSE)
      }
    }
  }

  item_id <- ifelse(is.null(item_id), "", paste0(", ids: [", paste(item_id, collapse = ","), "]"))
  board_id <- ifelse(is.null(board_id), "", paste0(", ids: [", paste(board_id, collapse = ","), "]"))
  column_id <- ifelse(is.null(column_id), "", column_id)
  column_title <- ifelse(is.null(column_title), "", column_title)
  column_type <- ifelse(is.null(column_type), "", column_type)

  boards <- monday_query(paste0(
    "{boards(limit: 10000", board_id, "){id, name, items(limit: 10000", item_id, "){id, name, column_values{id, title, type, value, text}}}}"
  ))

  if(raw){
    return(boards)
  }

  boards <- boards$data$boards

  if(!is.null(boards)){

    data <- NULL
    for(b in 1:(length(boards))){
      for(i in 1:(length(boards[[b]]$items))){
        for(j in 1:(length(boards[[b]]$items[[i]]$column_values))){
          cl = boards[[b]]$items[[i]]$column_values[[j]]

          if((column_id == "" | column_id == cl['id']) &
             (column_title == "" | column_title == cl['title']) &
             (column_type == "" | column_type == cl['type'])){

            data <- rbind.fill(
              data,
              data.frame(
                board_id = nullToNA(boards[[b]]$id),
                board_name = nullToNA(boards[[b]]$name),
                item_id = nullToNA(boards[[b]]$items[[i]]$id),
                item_name = nullToNA(boards[[b]]$items[[i]]$name),
                column_id = nullToNA(cl['id'][[1]]),
                column_title = nullToNA(cl['title'][[1]]),
                column_type = nullToNA(cl['type'][[1]]),
                column_text = nullToNA(cl['text'][[1]]),
                column_value = nullToNA(cl['value'][[1]]),
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }
    }

    if(is.null(data)){
      stop("No columns found matching the provided criteria", call. = FALSE)
    }

    return(data)
  }
}
