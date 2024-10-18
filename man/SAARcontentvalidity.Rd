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
    \item{\code{SAAR-B1}}{A numeric of expert rating to the SAAR on the Rubric B item 1.}
    \item{\code{SAAR-B2}}{A numeric of expert rating to the SAAR on the Rubric B item 2.}
    \item{\code{SAAR-B3}}{A numeric of expert rating to the SAAR on the Rubric B item 3.}
    \item{\code{SAAR-B4}}{A numeric of expert rating to the SAAR on the Rubric B item 4.}
    \item{\code{SAAR-B5}}{A numeric of expert rating to the SAAR on the Rubric B item 5.}
    \item{\code{SAAR-B6}}{A numeric of expert rating to the SAAR on the Rubric B item 6.}
    \item{\code{SAAR-B7}}{A numeric of expert rating to the SAAR on the Rubric B item 7.}
    \item{\code{SAAR-B8}}{A numeric of expert rating to the SAAR on the Rubric B item 8.}
    \item{\code{SAAR-B9}}{A numeric of expert rating to the SAAR on the Rubric B item 9.}
    \item{\code{SAAR-F1}}{A numeric of expert rating to the SAAR on the Rubric F item 1.}
    \item{\code{SAAR-F2}}{A numeric of expert rating to the SAAR on the Rubric F item 2.}
    \item{\code{SAAR-G1}}{A numeric of expert rating to the SAAR on the Rubric G item 1.}
    \item{\code{SAAR-G2}}{A numeric of expert rating to the SAAR on the Rubric G item 2.}
    \item{\code{SAAR-G3}}{A numeric of expert rating to the SAAR on the Rubric G item 3.}
    \item{\code{SAAR-G4}}{A numeric of expert rating to the SAAR on the Rubric G item 4.}
    \item{\code{SAAR-G5}}{A numeric of expert rating to the SAAR on the Rubric G item 5.}
  }
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("SAARcontentvalidity")
# Open the data
SAARcontentvalidity
}
