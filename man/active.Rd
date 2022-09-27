\name{active}
\alias{active}
\title{
    Quick summary of blau object.
}
\description{
    Identifies measures that have been computed.
}
\usage{
active(blauObj)
}
\arguments{
  \item{blauObj}{
    An object of class blau initialized with the function \code{blau}.}
}
\details{
    Since measures computed with many functions are stored in the blau object itself, it is helpful to quickly see which elements are active.
}
\value{
A string indicating the active elements.
}
\examples{
data(TwoCities)
b <- blau(TwoCities, node.ids = 'respID', ecology.ids = 'samp')
b <- niches(b, dev.range = rep(1.5, 10)) # 10 is the number of dimensions
active(b) #will produce command line output
}
