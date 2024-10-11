\name{teachersjudgment}
\alias{teachersjudgment}
\docType{data}
\title{Teachers' judgment dataset}
\description{
In this study, physics teachers are studied to predict the binary state of their students' performance at the final second semester.
}
\usage{data(teachersjudgment)}
\format{
  A data frame with 497 rows of final test assessments and prediction reported by physics teachers.
  \describe{
    \item{\code{FINTEST1}}{Students' score on the final test at the first semester using teachers developed items.}
    \item{\code{FINTEST2}}{Students' score on the final test at the second semester using teachers developed items.}
    \item{\code{TEACHPRED}}{Students' performance state as predicted by physics teachers intuitively. 1 = higher ability, 0 = lower ability.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1}, (2024). doi: 10.17632/88d7m2fv7p.1.
}
\references{
Zhu, C. & Urhahne, D. Temporal stability of teachers’ judgment accuracy of students’ motivation, emotion, and achievement. \emph{European Journal of Psychology of Education 36}, 319–337 (2021).
}
\examples{
data(teachersjudgment)
}
\keyword{datasets}
