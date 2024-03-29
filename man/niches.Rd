\name{niches}
\alias{niches}
\title{
Locate individuals in organizational niches in Blau space
}
\description{
Takes an object created with function blau and locates individuals in organizational niches. Automatically handles multiple ecologies by performing niche calculations separately for each ecology.
}
\usage{
niches(blauObj, dev.range, ecologies.off = FALSE)
}
\arguments{
  \item{blauObj}{
    An object created with the function \code{blau}.
}
  \item{dev.range}{
    When creating niches, indicates standard deviation around the mean in each dimension to include in niche. A larger value will make niches larger and therefore include more individuals.
}
  \item{ecologies.off}{
    Defaults to \code{FALSE}. If set to \code{TRUE}, treats all individuals as in the same ecology, even if ecology identifiers (\code{ecology.ids}) have been specified.}
}
\details{
Creates niches for each organization within each ecology. Niches are hypercubes in euclidean space that define the area a member-seeking organization is likely to recruit members from. Consider a given organization: its members have various traits such as age, income, and work experience. We find the mean point in n-dimensional space for an organization. For each dimension, we extend \code{dev.range*SD} out from this mean in both positive and negative directions. Doing this for each dimension defines the niche for the given organization within the given ecology.

Once we define niche dimensions, we examine the demographic information for each individual and indicate whether the individual is inside the niche. This information is stored in the matrix \code{blauObject$isInNiche}.

Each ecology may have different niche boundaries for a given organization. This heterogeneity is important for examining how different organizations recruit in different ecologies (e.g. cities, schools, etc.).
}
\value{
\item{obj}{Adds \code{topbounds}, \code{lowbounds}, and \code{isInNiche} to the Blau object.}
}
\examples{
data(TwoCities)
b <- blau(TwoCities, node.ids = 'respID', ecology.ids = 'samp')
#compute niches
b0 <- niches(b, dev.range = rep(1.5, 10)) # 10 is the number of dimensions
#change std. dev used for calculating niches
b1 <- niches(b, dev.range = rep(1.0, 10)) # 10 is the number of dimensions
#treat everyone as members of one ecology
b2 <- niches(b, dev.range = rep(1.0, 10), ecologies.off = TRUE) # 10 is the number of dimensions
}
