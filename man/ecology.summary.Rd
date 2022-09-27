\name{ecology.summary}
\alias{ecology.summary}
\title{
Summarizes the distribution of individuals across niches.
}
\description{
Outputs a niche-by-niche matrix for each ecology where each cell, [A,B], corresponds to the number of individuals who are in both niche A and niche B. Diagonal elements, [A,A], contain the number of individuals exclusively in ecology A.
}
\usage{
ecology.summary(blauObj, percent = FALSE)
}
\arguments{
  \item{blauObj}{
    An object of class blau initialized with the function \code{blau}. The function niches must have been called previously by the user. 
}
  \item{percent}{
    Defaults to \code{FALSE}. If \code{TRUE}, expresses output in terms of percentages (relative to the total number of people in an ecology).
}
}
\details{
The purpose of this function is to give the analyst a sense of the structure of the ecology. At one extreme the niches may be completely overlapping and at the other the niches may be completely separate. The former condition suggests fierce competition, whereas the later suggests strong localization. A situation in the middle indicates a particularly interesting competitive situation.
}
\value{
Returns an object of class \code{data.frame} that contains a niche-by-niche matrix for each ecology. Ecology and niche identifiers are presented in the first two columns, followed by the niche-by-niche matrix. Matrices are stacked vertically.
}
\examples{
data(TwoCities)
b <- blau(TwoCities, node.ids = 'respID', ecology.ids = 'samp')
b <- niches(b, dev.range = rep(1.5, 10)) # 10 is the number of dimensions
ecology.summary <- ecology.summary(b)
}
