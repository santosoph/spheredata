\name{CLASS}
\alias{CLASS}
\docType{data}
\title{The Colorado Learning Attitudes about Science Survey (CLASS) dataset}
\description{
The CLASS originally includes 42 attitudinal items measuring students' attitude toward physics learning within five categories of Likert scale ranging from strongly disagree (1) to strongly agree (5). In this package, thirty-six CLASS items have been preprocessed based on the scoring rule as suggested by Adams et al. (2006).
}
\usage{data(CLASS)}
\format{
  A data frame of students' responses on the following 36 CLASS items (after preprocessed using the scoring rule).
  \describe{
    \item{\code{CLASS1}}{A student's response on the CLASS item number 1.}
    \item{\code{CLASS2}}{A student's response on the CLASS item number 2.}
    \item{\code{CLASS3}}{A student's response on the CLASS item number 3.}
    \item{\code{CLASS5}}{A student's response on the CLASS item number 5.}
    \item{\code{CLASS6}}{A student's response on the CLASS item number 6.}
    \item{\code{CLASS8}}{A student's response on the CLASS item number 8.}
    \item{\code{CLASS10}}{A student's response on the CLASS item number 10.}
    \item{\code{CLASS11}}{A student's response on the CLASS item number 11.}
    \item{\code{CLASS12}}{A student's response on the CLASS item number 12.}
    \item{\code{CLASS13}}{A student's response on the CLASS item number 13.}
    \item{\code{CLASS14}}{A student's response on the CLASS item number 14.}
    \item{\code{CLASS15}}{A student's response on the CLASS item number 15.}
    \item{\code{CLASS16}}{A student's response on the CLASS item number 16.}
    \item{\code{CLASS17}}{A student's response on the CLASS item number 17.}
    \item{\code{CLASS18}}{A student's response on the CLASS item number 18.}
    \item{\code{CLASS19}}{A student's response on the CLASS item number 19.}
    \item{\code{CLASS20}}{A student's response on the CLASS item number 20.}
    \item{\code{CLASS21}}{A student's response on the CLASS item number 21.}
    \item{\code{CLASS22}}{A student's response on the CLASS item number 22.}
    \item{\code{CLASS23}}{A student's response on the CLASS item number 23.}
    \item{\code{CLASS24}}{A student's response on the CLASS item number 24.}
    \item{\code{CLASS25}}{A student's response on the CLASS item number 25.}
    \item{\code{CLASS26}}{A student's response on the CLASS item number 26.}
    \item{\code{CLASS27}}{A student's response on the CLASS item number 27.}
    \item{\code{CLASS28}}{A student's response on the CLASS item number 28.}
    \item{\code{CLASS29}}{A student's response on the CLASS item number 29.}
    \item{\code{CLASS30}}{A student's response on the CLASS item number 30.}
    \item{\code{CLASS32}}{A student's response on the CLASS item number 32.}
    \item{\code{CLASS34}}{A student's response on the CLASS item number 34.}
    \item{\code{CLASS35}}{A student's response on the CLASS item number 35.}
    \item{\code{CLASS36}}{A student's response on the CLASS item number 36.}
    \item{\code{CLASS37}}{A student's response on the CLASS item number 37.}
    \item{\code{CLASS38}}{A student's response on the CLASS item number 38.}
    \item{\code{CLASS39}}{A student's response on the CLASS item number 39.}
    \item{\code{CLASS40}}{A student's response on the CLASS item number 40.}
    \item{\code{CLASS42}}{A student's response on the CLASS item number 42.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1, (2024)}. doi: 10.17632/88d7m2fv7p.1
}
\references{
Adams, W. K. et al. New instrument for measuring student beliefs about physics and learning physics: The Colorado Learning Attitudes about Science Survey. \emph{Physical Review Special Topics - Physics Education Research 2}, 010101 (2006).
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("CLASS")
# Open the data
CLASS
}
