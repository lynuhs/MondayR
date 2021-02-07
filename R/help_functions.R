#' Help functions to perform basic tasks
#'
#' Simple functions to do stuff
#'
#' @keywords internal
#' @family help functions
#' @noRd
nullToNA <- function(x){
  return(ifelse(is.null(x), NA, x))
}
