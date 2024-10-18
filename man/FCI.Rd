\name{FCI}
\alias{FCI}
\docType{data}
\title{The Force Concept Inventory (FCI) dataset}
\description{
The focus of the FCI is intended to measure students’ conceptual understanding of Newtonian mechanics. It comprises of 30 multiple-choice items with five possible responses (a key and four distractors).
}
\usage{data(FCI)}
\format{
  A data frame of students' responses on the following 30 FCI multiple choice items.
  \describe{
    \item{\code{FCI1}}{A student's response on the FCI item number 1.}
    \item{\code{FCI2}}{A student's response on the FCI item number 2.}
    \item{\code{FCI3}}{A student's response on the FCI item number 3.}
    \item{\code{FCI4}}{A student's response on the FCI item number 4.}
    \item{\code{FCI5}}{A student's response on the FCI item number 5.}
    \item{\code{FCI6}}{A student's response on the FCI item number 6.}
    \item{\code{FCI7}}{A student's response on the FCI item number 7.}
    \item{\code{FCI8}}{A student's response on the FCI item number 8.}
    \item{\code{FCI9}}{A student's response on the FCI item number 9.}
    \item{\code{FCI10}}{A student's response on the FCI item number 10.}
    \item{\code{FCI11}}{A student's response on the FCI item number 11.}
    \item{\code{FCI12}}{A student's response on the FCI item number 12.}
    \item{\code{FCI13}}{A student's response on the FCI item number 13.}
    \item{\code{FCI14}}{A student's response on the FCI item number 14.}
    \item{\code{FCI15}}{A student's response on the FCI item number 15.}
    \item{\code{FCI16}}{A student's response on the FCI item number 16.}
    \item{\code{FCI17}}{A student's response on the FCI item number 17.}
    \item{\code{FCI18}}{A student's response on the FCI item number 18.}
    \item{\code{FCI19}}{A student's response on the FCI item number 19.}
    \item{\code{FCI20}}{A student's response on the FCI item number 20.}
    \item{\code{FCI21}}{A student's response on the FCI item number 21.}
    \item{\code{FCI22}}{A student's response on the FCI item number 22.}
    \item{\code{FCI23}}{A student's response on the FCI item number 23.}
    \item{\code{FCI24}}{A student's response on the FCI item number 24.}
    \item{\code{FCI25}}{A student's response on the FCI item number 25.}
    \item{\code{FCI26}}{A student's response on the FCI item number 26.}
    \item{\code{FCI27}}{A student's response on the FCI item number 27.}
    \item{\code{FCI28}}{A student's response on the FCI item number 28.}
    \item{\code{FCI29}}{A student's response on the FCI item number 29.}
    \item{\code{FCI30}}{A student's response on the FCI item number 30.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1}, (2024). doi: 10.17632/88d7m2fv7p.1.
}
\references{
Hestenes, D., Wells, M. & Swackhamer, G. Force concept inventory. \emph{Phys Teach 30}, 141–158 (1992).
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("FCI")
# Open the data
FCI
}
