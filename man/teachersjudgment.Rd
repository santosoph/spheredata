\name{teachersjudgment}
\alias{teachersjudgment}
\docType{data}
\title{Teachers' judgment dataset}
\description{
In this study, physics teachers are asked to predict the binary state of their students' performance in the final semester
}
\usage{data(teachersjudgment)}
\format{
  A data frame with 497 observations on the following 2 physics identity items.
  \describe{
    \item{\code{FINTEST1}}{Students' score on the final test at the first semester using teachers developed items.}
    \item{\code{FINTEST2}}{Students' score on the final test at the second semester using teachers developed items.}
    \item{\code{TEACHPRED}}{Students' state of performance as predicted by physics teachers. 1 = higher ability, 0 = lower ability.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). Mendeley Data, V1, (2024). doi: 10.17632/88d7m2fv7p.1.
}
\references{
Zhu, C. & Urhahne, D. Temporal stability of teachers’ judgment accuracy of students’ motivation, emotion, and achievement. European Journal of Psychology of Education 36, 319–337 (2021).
}
\examples{
data(teachersjudgment)
}
\keyword{datasets}
