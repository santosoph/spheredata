\name{CLASS}
\alias{CLASS}
\docType{data}
\title{Colorado Learning Attitudes about Science Survey (CLASS) dataset}
\description{
The CLASS originally includes 42 attitudinal items measuring students' attitude toward physics learning within five categories of Likert scale ranging from strongly disagree (1) to strongly agree (5). In this package, we have preprocessed the data based on the scoring rule as suggested by Adams et al. (2006).
}
\usage{data(CLASS)}
\format{
  A data frame with 497 observations on the following 36 CLASS items (after preprocessed using the scoring rule).
  \describe{
    \item{\code{CLASS1}}{A significant problem in learning physics is being able to memorize all the information I need to know.}
    \item{\code{CLASS2}}{When I am solving a physics problem, I try to decide what would be a reasonable value for the answer.}
    \item{\code{CLASS3}}{I think about the physics I experience in everyday life.}
    \item{\code{CLASS5}}{After I study a topic in physics and feel that I understand it, I have difficulty solving problems on the same topic.}
    \item{\code{CLASS6}}{Knowledge in physics consists of many disconnected topics.}
    \item{\code{CLASS8}}{When I solve a physics problem, I locate an equation that uses the variables given in the problem and plug in the values.}
    \item{\code{CLASS10}}{There is usually only one correct approach to solving a physics problem.}
    \item{\code{CLASS11}}{I am not satisfied until I understand why something works the way it does.}
    \item{\code{CLASS12}}{I cannot learn physics if the teacher does not explain things well in class.}
    \item{\code{CLASS13}}{I do not expect physics equations to help my understanding of the ideas; they are just for doing calculations.}
    \item{\code{CLASS14}}{I study physics to learn knowledge that will be useful in my life outside of school.}
    \item{\code{CLASS15}}{If I get stuck on a physics problem my first try, I usually try to figure out a different way that works.}
    \item{\code{CLASS16}}{Nearly everyone is capable of understanding physics if they work at it.}
    \item{\code{CLASS17}}{Understanding physics basically means being able to recall something you’ve read or been shown.}
    \item{\code{CLASS18}}{There could be two different correct values to a physics problem if I use two different approaches.}
    \item{\code{CLASS19}}{To understand physics, I discuss it with friends and other students.}
    \item{\code{CLASS20}}{I do not spend more than five minutes stuck on a physics problem before giving up or seeking help from someone else.}
    \item{\code{CLASS21}}{If I don’t remember a particular equation needed to solve a problem on an exam, there’s nothing much I can do (legally!) to come up with it.}
    \item{\code{CLASS22}}{If I want to apply a method used for solving one physics problem to another problem, the problems must involve very similar situations.}
    \item{\code{CLASS23}}{In doing a physics problem, if my calculation gives a very different from what I’d expect, I’d trust the calculation rather than going back through the problem.}
    \item{\code{CLASS24}}{In physics, it is important for me to make sense out of formulas before I can use them correctly.}
    \item{\code{CLASS25}}{I enjoy solving physics problems.}
    \item{\code{CLASS26}}{In physics, mathematical formulas express meaningful relationships among measurable quantities.}
    \item{\code{CLASS27}}{It is important for the government to approve new scientific ideas before they can be widely accepted.}
    \item{\code{CLASS28}}{Learning physics changes my ideas about how the world works.}
    \item{\code{CLASS29}}{To learn physics, I only need to memorize solutions to sample problems.}
    \item{\code{CLASS30}}{Reasoning skills used to understand physics can be helpful to me in my everyday life.}
    \item{\code{CLASS32}}{Spending a lot of time understanding where formulas come from is a waste of time.}
    \item{\code{CLASS34}}{I can usually figure out a way to solve physics problems.}
    \item{\code{CLASS35}}{The subject of physics has little relation to what I experience in the real world}
    \item{\code{CLASS36}}{There are times I solve a physics problem more than one way to help my understanding.}
    \item{\code{CLASS37}}{To understand physics, I sometimes think about my personal experiences and relate them to the topic being analyzed.}
    \item{\code{CLASS38}}{It is possible to explain physics ideas without mathematical formulas.}
    \item{\code{CLASS39}}{When I solve a physics problem, I explicitly think about which physics ideas apply to the problem.}
    \item{\code{CLASS40}}{If I get stuck on a physics problem, there is no chance I’ll figure it out on my own.}
    \item{\code{CLASS42}}{When studying physics, I relate the important information to what I already know rather than just memorizing it the way it is presented.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). Mendeley Data, V1, (2024). doi: 10.17632/88d7m2fv7p.1
}
\references{
Adams, W. K. et al. New instrument for measuring student beliefs about physics and learning physics: The Colorado Learning Attitudes about Science Survey. Physical Review Special Topics - Physics Education Research 2, 010101 (2006).
}
\examples{
data(CLASS)
}
\keyword{datasets}
