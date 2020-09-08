
base_url <- "https://firestore.googleapis.com/v1/"

#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connexion")
}

#' @importFrom attempt stop_if_not
#' @importFrom httr status_code
check_status <- function(res){
  attempt::stop_if_not(.x = httr::status_code(res),
              .p = ~ .x == 200,
              msg = "The API returned an error")
}

rm_string <- function(x, pattern) {
  gsub(pattern, '', x, fixed=T)
}

#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
#' @noRd
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
#' @noRd
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}
