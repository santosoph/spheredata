#' Compute the Aiken's V index of content validity
#'
#' Aiken's \eqn{V} is a statistical measure of content validity index on a single
#' item (Aiken, 1980). This measure could be defined as follow.
#' \deqn{\displaystyle V=\frac{\bar{X}-l}{k}}
#' where \eqn{\bar{X}} represents the sample mean of the judges’ ratings,
#' \eqn{l} represents the lowest possible rating, and \eqn{k} represents the
#' range of possible values of the rating scale used (e.g., a scale having
#' possible values extending from 1 to 5 has \eqn{l=1} and \eqn{k = 5-1 = 4}).
#' Then, Penfield and Giacobbi (2004) suggest a method to compute the confidence
#' interval of Aiken's V index using the formula below.
#' \deqn{\displaystyle L=\frac{2nkV+z^2-z\sqrt{4nkV(1-V)+z^2}}{2(nk+z^2)}}
#' \deqn{\displaystyle U=\frac{2nkV+z^2+z\sqrt{4nkV(1-V)+z^2}}{2(nk+z^2)}}
#' where \eqn{L} and \eqn{U} are the lower and upper limit of Aiken's \eqn{V}
#' index within a \eqn{C%} confidence interval, and the \eqn{z} is a critical
#' value of a standard normal distribution such that \eqn{C%} of the area of
#' the distribution lies between \eqn{-z} and \eqn{z} (e.g., for a 95%
#' confidence interval \eqn{z = 1.96}).
#'
#' @param data a dataframe of categorical value from expert judgment to the item content validity.
#' @param max_cat a maximum category point of used rating scale (the default value is 4).
#' @param min_cat a minimum category point of used rating scale (the default value is 1).
#' @param CI the default value of confidence interval is 0.95. It can be set to preferred confidence interval.
#'
#' @return a dataframe of content validity index of each item as calculated using the Aiken's formula
#' @export aikenV
#'
#' @references Aiken, L.R. Content Validity and Reliability of Single Items or Questionnaires. \emph{Educational and Psychological Measurement 40}, 955-959 (1980).
#' @references Penfield, R.D. & Giacobbi, P.R. Applying a Score Confidence Interval to Aiken’s Item Content-Relevance Index. \emph{Measurement in Physical Education and Exercise Science 8}, 4, 213-225 (2004).
#' @importFrom stats qnorm
#' @examples
#' # In this example, we define a dataframe describing the rating of ten
#' # imaginary items as assessed by seven artificial experts. The minimum point
#' # of the rating scale is 1, and the maximum point that could be given by
#' # those experts is 4.
#'
#' df <- data.frame(item1 = c(3,3,3,4,4,4,3),
#'                  item2 = c(2,4,3,2,4,4,4),
#'                  item3 = c(4,3,3,2,4,4,3),
#'                  item4 = c(3,2,3,3,4,3,3),
#'                  item5 = c(4,4,4,3,3,3,3),
#'                  item6 = c(3,3,3,4,3,3,4),
#'                  item7 = c(4,4,4,3,4,4,4),
#'                  item8 = c(3,3,4,4,4,4,4),
#'                  item9 = c(4,4,4,3,4,4,4),
#'                  item10 = c(4,3,4,4,3,3,4))
#'
#'# Compute the Aiken's V
#'aikenV(df)

aikenV <- function(data, max_cat=4, min_cat=1, CI=0.95){
  n <- colSums(data)
  k <- (max_cat-min_cat)

  V <- round(((n/nrow(data))-min_cat)/k,2)
  z <- qnorm(1-(1-CI)/2)

  A <- (2*n*k*V) + z^2
  B <- z*sqrt(4*n*k*V*(1-V)+(z^2))
  C <- 2*(n*k+z^2)

  lV <- round((A-B)/C,2)
  uV <- round((A+B)/C,2)

  return(data.frame("AikenV" = V, "lowerCI" = lV, "upperCI" = uV))
}
