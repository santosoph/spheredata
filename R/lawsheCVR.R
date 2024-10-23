#' Compute the Lawshe's content validity ratio (CVR)
#'
#' Lawshe (1975) proposed an index to quantify content validity of items as
#' assessed by experts. He coined the index as content validity ratio
#' (\eqn{CVR}) that can be calculated using the following formula.
#' \deqn{\displaystyle CVR=\frac{n_e - \frac{N}{2}}{\frac{N}{2}}}
#' where \eqn{n_e} is the number of experts identifying an item as essential.
#' In this function, we define a cutoff value using two methods. The first is
#' "max" calculated by searching the maximum value of the used rating scale
#' ("max_cat") and then divide it by two. The second method is "min" by using
#' the minimum value of the used rating scale ("min_cat") as the cutoff
#' criteria.
#'
#' @param data a dataframe of categorical value from expert judgment to the item content validity.
#' @param max_cat a maximum category point of used rating scale (the default value is 4).
#' @param min_cat a minimum category point of used rating scale (the default value is 1).
#' @param method a method to determine cutoff value between essential and non-essential items
#'
#' @return a dataframe of CVR of each item as calculated using the Lawshe's formula
#' @export lawsheCVR
#'
#' @references Gilbert, G.E. & Prion, S. Making Sense of Methods and Measurement: Lawshe's Content Validity Index. \emph{Clinical Simulation in Nursing 12}, 530-531 (2016).
#' @references Lawshe, C.H. A Quantitative Approach of Content Validity. \emph{Personnel Psychology 28}, 563-575 (1975).
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
#'# Compute the Lawshe's CVR
#'lawsheCVR(df)

lawsheCVR <- function(data, max_cat=4, min_cat=1, method = "max"){

  if (method == "max"){
    cutoff <- max_cat/2
  } else if(method == "min"){
    cutoff <- min_cat
  }

  N <-  nrow(data)
  n_e <- apply(data,2,function(row) sum(row > cutoff))
  cvr <- round((2*n_e/N) - 1,2)

  return(data.frame("CVR" = cvr))
}
