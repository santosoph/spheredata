\name{SAAR}
\alias{SAAR}
\docType{data}
\title{Scientific Abilities Assessment Rubrics (SAAR) dataset}
\description{
SAAR is developed as a qualitative scoring rubric used to measure students’ scientific abilities within the environment of physics laboratory. A rating scale with four categories (1–4) is used in this dataset to describe students’ work (1, missing; 2, inadequate; 3, needs some improvement; and 4, adequate) and devised descriptions of student work that could merit a particular score. In this package, we decide to merely include some abilities (the ability to design & conduct an observational experiment (Rubric B),the ability to communicate scientific ideas (Rubric F), and the ability to collect and analyze experimental data (Rubric G)) since they could be more relevant and important for the high school physics laboratory.
}
\usage{data(SAAR)}
\format{
  A data frame with 497 observations on the following 16 SAAR observation items in the physics laboratory.
  \describe{
    \item{\code{SAAR-B1}}{A numeric of the observation using the SAAR on the Rubric B item 1.}
    \item{\code{SAAR-B2}}{A numeric of the observation using the SAAR on the Rubric B item 2.}
    \item{\code{SAAR-B3}}{A numeric of the observation using the SAAR on the Rubric B item 3.}
    \item{\code{SAAR-B4}}{A numeric of the observation using the SAAR on the Rubric B item 4.}
    \item{\code{SAAR-B5}}{A numeric of the observation using the SAAR on the Rubric B item 5.}
    \item{\code{SAAR-B6}}{A numeric of the observation using the SAAR on the Rubric B item 6.}
    \item{\code{SAAR-B7}}{A numeric of the observation using the SAAR on the Rubric B item 7.}
    \item{\code{SAAR-B8}}{A numeric of the observation using the SAAR on the Rubric B item 8.}
    \item{\code{SAAR-B9}}{A numeric of the observation using the SAAR on the Rubric B item 9.}
    \item{\code{SAAR-F10}}{A numeric of the observation using the SAAR on the Rubric F item 1.}
    \item{\code{SAAR-F11}}{A numeric of the observation using the SAAR on the Rubric F item 2.}
    \item{\code{SAAR-G12}}{A numeric of the observation using the SAAR on the Rubric G item 1.}
    \item{\code{SAAR-G13}}{A numeric of the observation using the SAAR on the Rubric G item 2.}
    \item{\code{SAAR-G14}}{A numeric of the observation using the SAAR on the Rubric G item 3.}
    \item{\code{SAAR-G15}}{A numeric of the observation using the SAAR on the Rubric G item 4.}
    \item{\code{SAAR-G16}}{A numeric of the observation using the SAAR on the Rubric G item 5.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). Mendeley Data, V1, (2024). doi: 10.17632/88d7m2fv7p.1
}
\references{
Etkina, E. et al. Scientific abilities and their assessment. Physical Review Special Topics - Physics Education Research 2, 020103 (2006).
}
\examples{
data(SAAR)
}
\keyword{datasets}
