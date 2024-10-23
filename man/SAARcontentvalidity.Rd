\name{SAARcontentvalidity}
\alias{SAARcontentvalidity}
\docType{data}
\title{The Scientific Abilities Assessment Rubrics (SAAR) content validity dataset}
\description{
In this data, five PER experts rated the content validity of the STPFASL.
}
\usage{data(SAARcontentvalidity)}
\format{
  A data frame of expert ratings on the content validity of 16 SAAR observation items.
  \describe{
    \item{\code{ID}}{An anonymized expert identity.}
    \item{\code{SAARB1}}{A numeric of expert rating on the SAAR Rubric B item 1.}
    \item{\code{SAARB2}}{A numeric of expert rating on the SAAR Rubric B item 2.}
    \item{\code{SAARB3}}{A numeric of expert rating on the SAAR Rubric B item 3.}
    \item{\code{SAARB4}}{A numeric of expert rating on the SAAR Rubric B item 4.}
    \item{\code{SAARB5}}{A numeric of expert rating on the SAAR Rubric B item 5.}
    \item{\code{SAARB6}}{A numeric of expert rating on the SAAR Rubric B item 6.}
    \item{\code{SAARB7}}{A numeric of expert rating on the SAAR Rubric B item 7.}
    \item{\code{SAARB8}}{A numeric of expert rating on the SAAR Rubric B item 8.}
    \item{\code{SAARB9}}{A numeric of expert rating on the SAAR Rubric B item 9.}
    \item{\code{SAARF10}}{A numeric of expert rating on the SAAR Rubric F item 1.}
    \item{\code{SAARF11}}{A numeric of expert rating on the SAAR Rubric F item 2.}
    \item{\code{SAARG12}}{A numeric of expert rating on the SAAR Rubric G item 1.}
    \item{\code{SAARG13}}{A numeric of expert rating on the SAAR Rubric G item 2.}
    \item{\code{SAARG14}}{A numeric of expert rating on the SAAR Rubric G item 3.}
    \item{\code{SAARG15}}{A numeric of expert rating on the SAAR Rubric G item 4.}
    \item{\code{SAARG16}}{A numeric of expert rating on the SAAR Rubric G item 5.}
  }
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("SAARcontentvalidity")
# Open the data
SAARcontentvalidity
}
