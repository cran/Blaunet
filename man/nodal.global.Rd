\name{nodal.global}
\alias{nodal.global}
\title{
Compute Blau statues for individuals across all niches
}
\description{
Computes three measures---total number of organizations the individual occupies, total number of niches a person occupies, and a string indicating the niches an individual occupies. 
}
\usage{
nodal.global(blauObj, dev.range, ecologies.off = FALSE)
}
\arguments{
  \item{blauObj}{
    An object of class blau initialized with the function \code{blau}. Individuals will automatically be placed in niches with the function \code{niches} if this has not been done manually.}
  \item{dev.range}{
    When creating niches, indicates standard deviation around the mean in each dimension to include in niche. A larger value will make niches larger and therefore include more individuals.
}
  \item{ecologies.off}{
    Defaults to \code{FALSE}. If set to \code{TRUE}, treats all individuals as in the same ecology, even if ecology identifiers (\code{ecology.ids}) have been specified. Will call the \code{niches} function and overwrite its output even if it has been manually called by the user.
}
}
\details{
The three measures computed provide information on each individual across all niches.

TotalOrgs: Total number of organizations the person is in. The lower bound is 0 and the upper bound is the maximum number of organizations in the ecology. 

Nicher:  Provides information on how many niches the person is in or how many organizations are competing for that individual (irrespective of actual membership).  The value of 0 indicates that a person in an outsider (is not in any niche). The value of 1 indicates that the person is an exclusive nicher, suggesting an organizational monopoly. A value of 2 or more indicates that the person is a manifolder or belongs to multiple niches and is a potential recruit for multiple organizations. 

Niches: a string indicating which niches the individual belongs to and containing niche column numbers separated by spaces. This provides an at-a-glance format and may be easily split up with string parsing functions.

}
\value{
A matrix object stored in object$nodalGlobal that contains the three measures in three columns. Row names are node names provided with the blau function.
}
\examples{
data(TwoCities)
b <- blau(TwoCities, node.ids = 'respID', ecology.ids = 'samp')
#will automatically call niches
b0 <- nodal.global(b, dev.range = rep(1.5, 10)) # 10 is the number of dimensions
#treat all individuals as in same ecology
b1 <- nodal.global(b, dev.range = rep(1.5, 10), ecologies.off = TRUE) 
# 10 is the number of dimensions in the command line above 
}
