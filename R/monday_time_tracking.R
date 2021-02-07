#' Get time tracking data
#'
#' Get time tracking data from your Monday.com boards
#'
#' @param board_id The id of a Monday.com board (optional)
#' @param raw Wether or not to return raw or structured data (defaults to FALSE)
#'
#' @import lubridate
#'
#' @export
#' @examples
#' monday_time_tracking(board_id = 1234)
monday_time_tracking <- function(board_id = NULL, raw = FALSE){
  if(is.null(board_id)){
    cat(crayon::red("Getting a list of all boards\n"))
    time_boards <- NULL
    boards <- monday_list_boards()$id

    if(length(boards) == 0){
      stop("No boards found", call. = FALSE)
    }

    for(i in 1:(length(boards))){
      if("duration" %in% monday_list_columns(boards[i])$type){
        time_boards <- c(time_boards, boards[i])
      }
    }

    board_id <- time_boards
  } else {
    monday_boards <- monday_list_boards()$id
    for(i in 1:(length(board_id))){
      if(!board_id[i] %in% monday_boards){
        stop(paste0("The board id '", board_id[i], "' does not exist in your account"), call. = FALSE)
      }
    }
  }

  tryCatch({
    cat(crayon::red("Downloading time tracking data for chosen board\n"))
    data <- monday_column_values(board_id = board_id, column_type = "duration")

    cat(crayon::red("All data downloaded successfully\n"))
  }, error = function(e){
    stop("Couldn't get the data from Monday.com, check your parameters and try again later", call. = FALSE)
  })

  if(raw){
    return(data)
  }

  detail <- NULL
  for(n in 1:nrow(data)){
    if(!is.na(data[n,'column_value'])){
      row_data <- jsonlite::fromJSON(data[n,'column_value'])
      for(i in 1:nrow(row_data$additional_value)){
        detail <- rbind.fill(
          detail,
          data.frame(
            board_id = data[n,'board_id'],
            board_name = data[n,'board_name'],
            item_id = data[n,'item_id'],
            item_name = data[n,'item_name'],
            id = row_data$additional_value[i,]$id,
            user = row_data$additional_value[i,]$started_user_id,
            started_at = as_datetime(row_data$additional_value[i,]$started_at, tz="GMT"),
            ended_at = as_datetime(row_data$additional_value[i,]$ended_at, tz="GMT"),
            minutes = as.integer(difftime(
              as_datetime(row_data$additional_value[i,]$ended_at, tz="GMT"),
              as_datetime(row_data$additional_value[i,]$started_at, tz="GMT"),
              units = "mins"
            )),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  return(detail)
}
