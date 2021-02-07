#' Get custom API request string
#'
#' Query Monday.com with json encoded string
#'
#' @param query Query parameters for the Graph API
#'
#' @import httr
#'
#'
#' @export
#' @examples
#' monday_query("{me{name, id}}")
monday_query <- function(query){
  url <- paste0("https://api.monday.com/v2/")
  tryCatch({
    data <- content(POST(
      url = url,
      add_headers(
        .headers = c(
          "Authorization" = MondayAuth$public_fields$token$credentials$access_token
        )
      ),
      body = list(
        query = query
      ),
      encode = "json"
    ))

    if(any(grepl("errors.code",names(unlist(data))))){
      cat(crayon::red("Error: Not an authorized API call"))
    } #else if(!is.null(data$errors)){
      #stop(paste0("Error: ", data$errors[[1]]$message), call. = FALSE)
    #}
    else {
      return(data)
    }

  }, error = function(e){
    cat(crayon::red(e))
  })
}
