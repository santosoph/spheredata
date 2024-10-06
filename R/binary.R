#' Compute the students' score
#'
#' Compute the students' score as binary/ dichotomous data
#' @param raw a dataframe of raw response data
#' @param key a dataframe of answer key
#'
#' @return a dataframe of dichotomous format of students' response data
#' @export binary
#'
#' @examples
#' # Import the FCI score and key data
#' library(spheredata)
#' data("FCI")
#' data("FCIkey")
#'
#' # Processing the Force Concept Inventory (FCI) data as dichotomous
#' binary(FCI, FCIkey)

binary <- function(raw, key){
  dichotomous <- t(apply(raw,1,function(X){ifelse(X==(key),1,0)}))
  dichotomous
}
