\name{SAAR}
\alias{SAAR}
\docType{data}
\title{The Scientific Abilities Assessment Rubrics (SAAR) dataset}
\description{
The SAAR is developed as a qualitative scoring rubric intended to measure students’ scientific abilities within the environment of physics laboratory. A rating scale with four categories (1–4) is used in this dataset to describe the students’ work in the laboratory (1, missing; 2, inadequate; 3, needs some improvement; and 4, adequate) and devised descriptions of student work that could merit a particular score. In this package, we merely measure some abilities from the SAAR since they could be more relevant and important for the high school physics laboratory. They are the ability to design & conduct an observational experiment (Rubric B),the ability to communicate scientific ideas (Rubric F), and the ability to collect and analyze experimental data (Rubric G).
}
\usage{data(SAAR)}
\format{
  A data frame of students' performance in the physics laboratory measured by the following 16 SAAR observation items.
  \describe{
    \item{\code{SAARB1}}{An observation using SAAR on the Rubric B item 1.}
    \item{\code{SAARB2}}{An observation using SAAR on the Rubric B item 2.}
    \item{\code{SAARB3}}{An observation using SAAR on the Rubric B item 3.}
    \item{\code{SAARB4}}{An observation using SAAR on the Rubric B item 4.}
    \item{\code{SAARB5}}{An observation using SAAR on the Rubric B item 5.}
    \item{\code{SAARB6}}{An observation using SAAR on the Rubric B item 6.}
    \item{\code{SAARB7}}{An observation using SAAR on the Rubric B item 7.}
    \item{\code{SAARB8}}{An observation using SAAR on the Rubric B item 8.}
    \item{\code{SAARB9}}{An observation using SAAR on the Rubric B item 9.}
    \item{\code{SAARF10}}{An observation using SAAR on the Rubric F item 1 (SAAR item 10).}
    \item{\code{SAARF11}}{An observation using SAAR on the Rubric F item 2 (SAAR item 11).}
    \item{\code{SAARG12}}{An observation using SAAR on the Rubric G item 1 (SAAR item 12).}
    \item{\code{SAARG13}}{An observation using SAAR on the Rubric G item 2 (SAAR item 13).}
    \item{\code{SAARG14}}{An observation using SAAR on the Rubric G item 3 (SAAR item 14).}
    \item{\code{SAARG15}}{An observation using SAAR on the Rubric G item 4 (SAAR item 15).}
    \item{\code{SAARG16}}{An observation using SAAR on the Rubric G item 5 (SAAR item 16).}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1}, (2024). doi: 10.17632/88d7m2fv7p.1
}
\references{
Etkina, E. et al. Scientific abilities and their assessment. \emph{Physical Review Special Topics - Physics Education Research 2}, 020103 (2006).
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("SAAR")
# Open the data
SAAR
}
