\name{FMCI}
\alias{FMCI}
\docType{data}
\title{The Fluid Mechanics Concept Inventory (FMCI) dataset}
\description{
The Fluid Mechanics Concept Inventory (FMCI) is developed in 2003 as a conceptual inventory to explore students' ideas of fluid mechanics concepts [see Martin et al. (2003)]. The FMCI administers 30 multiple choice items.
}
\usage{data(FMCI)}
\format{
  A data frame of students' responses on the following 30 FMCI multiple choice items. The FMCI numbering system starts from the item 3.
  \describe{
    \item{\code{FMCI3}}{A student's response on the FMCI item number 3.}
    \item{\code{FMCI4}}{A student's response on the FMCI item number 4.}
    \item{\code{FMCI5}}{A student's response on the FMCI item number 5.}
    \item{\code{FMCI6}}{A student's response on the FMCI item number 6.}
    \item{\code{FMCI7}}{A student's response on the FMCI item number 7.}
    \item{\code{FMCI8}}{A student's response on the FMCI item number 8.}
    \item{\code{FMCI9}}{A student's response on the FMCI item number 9.}
    \item{\code{FMCI10}}{A student's response on the FMCI item number 10.}
    \item{\code{FMCI11}}{A student's response on the FMCI item number 11.}
    \item{\code{FMCI12}}{A student's response on the FMCI item number 12.}
    \item{\code{FMCI13}}{A student's response on the FMCI item number 13.}
    \item{\code{FMCI14}}{A student's response on the FMCI item number 14.}
    \item{\code{FMCI15}}{A student's response on the FMCI item number 15.}
    \item{\code{FMCI16}}{A student's response on the FMCI item number 16.}
    \item{\code{FMCI17}}{A student's response on the FMCI item number 17.}
    \item{\code{FMCI18}}{A student's response on the FMCI item number 18.}
    \item{\code{FMCI19}}{A student's response on the FMCI item number 19.}
    \item{\code{FMCI20}}{A student's response on the FMCI item number 20.}
    \item{\code{FMCI21}}{A student's response on the FMCI item number 21.}
    \item{\code{FMCI22}}{A student's response on the FMCI item number 22.}
    \item{\code{FMCI23}}{A student's response on the FMCI item number 23.}
    \item{\code{FMCI24}}{A student's response on the FMCI item number 24.}
    \item{\code{FMCI25}}{A student's response on the FMCI item number 25.}
    \item{\code{FMCI26}}{A student's response on the FMCI item number 26.}
    \item{\code{FMCI27}}{A student's response on the FMCI item number 27.}
    \item{\code{FMCI28}}{A student's response on the FMCI item number 28.}
    \item{\code{FMCI29}}{A student's response on the FMCI item number 29.}
    \item{\code{FMCI30}}{A student's response on the FMCI item number 30.}
    \item{\code{FMCI31}}{A student's response on the FMCI item number 31.}
    \item{\code{FMCI32}}{A student's response on the FMCI item number 32.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1}, (2024). doi: 10.17632/88d7m2fv7p.1.
}
\references{
Martin, J., Mitchell, J. & Newell, T. Development of a concept inventory for fluid mechanics. in \emph{Proceedings of the 33rd Annual Frontiers in Education 2003 vol. 1 T3D} (IEEE, 2003).
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("FMCI")
# Open the data
FMCI
}
