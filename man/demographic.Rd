\name{demographic}
\alias{demographic}
\docType{data}
\title{Students' demographic dataset}
\description{
This dataset is used to describe the students' contexts participating in the study.
}
\usage{data(demographic)}
\format{
  A data frame with 497 observations on the following 13 demographic items.
  \describe{
    \item{\code{STUDID}}{Students' identity. The first letter denotes the school code. The second one coins the students' cohort within schools. Three last numbers for their alphabetical orders.}
    \item{\code{SCH}}{School sample participated in the study.}
    \item{\code{COH}}{Students' class within schools. 1 = Group A, 2 = Group B, 3 = Group C, 4 = Group D}
    \item{\code{GDR}}{Students' gender. 1 = Male, 2 = Female}
    \item{\code{AGE}}{Students' age. 1 = 13-14 years, 2 = 15-16 years, 3 = 17-18 years, 4 = 19-20 years}
    \item{\code{FATHOCC}}{Father's occupation. 1 = Entrepreneur, 2 = Farmer, 3 = Armed force, 4 = Private employee, 5 = Educator, 6 = Medicine, 7 = Civil servant, 8 = Unemployed, 9 = Others}
    \item{\code{MOTHOCC}}{Mother’s occupation. 1 = Entrepreneur, 2 = Farmer, 3 = Armed force, 4 = Private employee, 5 = Educator, 6 = Medicine, 7 = Civil servant, 8 = Unemployed, 9 = Others}
    \item{\code{FATHEDU}}{Father's education. 1 = Graduate, 2 = Undergraduate, 3 = Vocational, 4 = High school, 5 = Junior high school, 6 = Elementary, 7 = Unfinished education, 8 = Out of formal education}
    \item{\code{MOTHEDU}}{Mother's education. 1 = Graduate, 2 = Undergraduate, 3 = Vocational, 4 = High school, 5 = Junior high school, 6 = Elementary, 7 = Unfinished education, 8 = Out of formal education}
    \item{\code{FATHINC}}{Father's monthly income. 1 = More than IDR10.000.000, 2 = IDR9.000.000-IDR10.000.000, 3 = IDR8.000.000-IDR9.000.000, 4 = IDR7.000.000-IDR8.000.000, 5 = IDR6.000.000-IDR7.000.000, 6 = IDR5.000.000-IDR6.000.000, 7 = IDR4.000.000-IDR5.000.000, 8 = IDR3.000.000-IDR4.000.000, 9 = IDR2.000.000-IDR3.000.000, 10 = IDR1.000.000-IDR2.000.000, 11= Less than IDR1.000.000, 12 = No income}
    \item{\code{MOTHINC}}{Mother's monthly income. 1 = More than IDR10.000.000, 2 = IDR9.000.000-IDR10.000.000, 3 = IDR8.000.000-IDR9.000.000, 4 = IDR7.000.000-IDR8.000.000, 5 = IDR6.000.000-IDR7.000.000, 6 = IDR5.000.000-IDR6.000.000, 7 = IDR4.000.000-IDR5.000.000, 8 = IDR3.000.000-IDR4.000.000, 9 = IDR2.000.000-IDR3.000.000, 10 = IDR1.000.000-IDR2.000.000, 11= Less than IDR1.000.000, 12 = No income}
    \item{\code{SIBL}}{Number of siblings belonged to the student. Zero means student as an only child.}
    \item{\code{DOM}}{Student's domicile from the school location. 1 = Inside the zoning area of the school. 2 = Outside the zoning area of the school.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). Mendeley Data, V1, (2024). doi: 10.17632/88d7m2fv7p.1
}
%\references{
%}
\examples{
data(demographic)
}
\keyword{datasets}
