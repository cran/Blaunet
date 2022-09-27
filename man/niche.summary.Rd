\name{niche.summary}
\alias{niche.summary}
\title{
    Summarizes information on each membership organization
}
\description{
    Aggregates information on each organization and returns five summary measures---the number of individuals in the organization, the number of individuals in the organization's niche, the number in the organization but not in the niche, the number of individuals exclusively in the niche, and the number of individuals who overlap with other niches.
}
\usage{
niche.summary(blauObj)
}
\arguments{
  \item{blauObj}{
    An object of class blau initialized with the function \code{blau}. The function \code{niches} must have been called previously by the user. 
}
}
\details{
The purpose of this function is to get a better sense of how the niches are organized within each ecology. 

The rows represent niches. The number of rows corresponds to the number of niches. 

Computes five measures:

OrgMem: computes how many people are in each of the organizations that structure the niche.

NicheMem: computes to how many people are in each of the niches 

NichExc: computes how many people are exclusive to that niche and only to that niche  (not in any other niche);  

NicheOvr: computes how many people are in 2 or more niches

MemExc: computes how many people are in the organization but not in the organization's niche. This happens because they are outside the demographic boundaries of the set standard deviations that are used to define the niche. 
}
\value{
Returns an object of class \code{data.frame} that contains the five summary measures as columns, along with two columns identifying the ecology and the niche corresponding to the information provided. 
}
\examples{
data(TwoCities)
b <- blau(TwoCities, node.ids = 'respID', ecology.ids = 'samp')
b <- niches(b, dev.range = rep(1.5, 10)) # 10 is the number of dimensions
niche.summary <- niche.summary(b)
}
