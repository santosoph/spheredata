\name{FMCE}
\alias{FMCE}
\docType{data}
\title{The Force and Motion Conceptual Evaluation (FMCE) dataset}
\description{
Similar with the FCI, the goal of the FMCE is also intended to measure students’ conceptual understanding of Newtonian mechanics. It comprises of 47 multiple-choice items.
}
\usage{data(FMCE)}
\format{
  A data frame of students' responses on the following 47 FMCE multiple choice items.
  \describe{
    \item{\code{FMCE1}}{A student's response on the FMCE item number 1.}
    \item{\code{FMCE2}}{A student's response on the FMCE item number 2.}
    \item{\code{FMCE3}}{A student's response on the FMCE item number 3.}
    \item{\code{FMCE4}}{A student's response on the FMCE item number 4.}
    \item{\code{FMCE5}}{A student's response on the FMCE item number 5.}
    \item{\code{FMCE6}}{A student's response on the FMCE item number 6.}
    \item{\code{FMCE7}}{A student's response on the FMCE item number 7.}
    \item{\code{FMCE8}}{A student's response on the FMCE item number 8.}
    \item{\code{FMCE9}}{A student's response on the FMCE item number 9.}
    \item{\code{FMCE10}}{A student's response on the FMCE item number 10.}
    \item{\code{FMCE11}}{A student's response on the FMCE item number 11.}
    \item{\code{FMCE12}}{A student's response on the FMCE item number 12.}
    \item{\code{FMCE13}}{A student's response on the FMCE item number 13.}
    \item{\code{FMCE14}}{A student's response on the FMCE item number 14.}
    \item{\code{FMCE15}}{A student's response on the FMCE item number 15.}
    \item{\code{FMCE16}}{A student's response on the FMCE item number 16.}
    \item{\code{FMCE17}}{A student's response on the FMCE item number 17.}
    \item{\code{FMCE18}}{A student's response on the FMCE item number 18.}
    \item{\code{FMCE19}}{A student's response on the FMCE item number 19.}
    \item{\code{FMCE20}}{A student's response on the FMCE item number 20.}
    \item{\code{FMCE21}}{A student's response on the FMCE item number 21.}
    \item{\code{FMCE22}}{A student's response on the FMCE item number 22.}
    \item{\code{FMCE23}}{A student's response on the FMCE item number 23.}
    \item{\code{FMCE24}}{A student's response on the FMCE item number 24.}
    \item{\code{FMCE25}}{A student's response on the FMCE item number 25.}
    \item{\code{FMCE26}}{A student's response on the FMCE item number 26.}
    \item{\code{FMCE27}}{A student's response on the FMCE item number 27.}
    \item{\code{FMCE28}}{A student's response on the FMCE item number 28.}
    \item{\code{FMCE29}}{A student's response on the FMCE item number 29.}
    \item{\code{FMCE30}}{A student's response on the FMCE item number 30.}
    \item{\code{FMCE31}}{A student's response on the FMCE item number 31.}
    \item{\code{FMCE32}}{A student's response on the FMCE item number 32.}
    \item{\code{FMCE33}}{A student's response on the FMCE item number 33.}
    \item{\code{FMCE34}}{A student's response on the FMCE item number 34.}
    \item{\code{FMCE35}}{A student's response on the FMCE item number 35.}
    \item{\code{FMCE36}}{A student's response on the FMCE item number 36.}
    \item{\code{FMCE37}}{A student's response on the FMCE item number 37.}
    \item{\code{FMCE38}}{A student's response on the FMCE item number 38.}
    \item{\code{FMCE39}}{A student's response on the FMCE item number 39.}
    \item{\code{FMCE40}}{A student's response on the FMCE item number 40.}
    \item{\code{FMCE41}}{A student's response on the FMCE item number 41.}
    \item{\code{FMCE42}}{A student's response on the FMCE item number 42.}
    \item{\code{FMCE43}}{A student's response on the FMCE item number 43.}
    \item{\code{FMCE44}}{A student's response on the FMCE item number 44.}
    \item{\code{FMCE45}}{A student's response on the FMCE item number 45.}
    \item{\code{FMCE46}}{A student's response on the FMCE item number 46.}
    \item{\code{FMCE47}}{A student's response on the FMCE item number 47.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1}, (2024). doi: 10.17632/88d7m2fv7p.1.
}
\references{
Thornton, R. K. & Sokoloff, D. R. Assessing student learning of Newton’s laws: The Force and Motion Conceptual Evaluation and the Evaluation of Active Learning Laboratory and Lecture Curricula. \emph{Am J Phys 66}, 338–352 (1998).
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("FMCE")
# Open the data
FMCE
}
