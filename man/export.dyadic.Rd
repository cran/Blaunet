\name{export.dyadic}
\alias{export.dyadic}
\title{
    Outputs dyadic level measures computed by the user
}
\description{
Takes any output from dyadic blau functions (dyadic) and presents it in one matrix for further analysis outside of Blaunet or output to disk.
}
\usage{
export.dyadic(blauObj)
}
\arguments{
  \item{blauObj}{
}
}
\details{
This function is useful to manipulate the computed dyadic Blau statuses and analyze them outside the Blaunet program, either within R or by exporting them to another statistical package. 
}
\value{
Returns an object of class data.frame that includes all output from function dyadic. The user must manually call the \code{dyadic} function prior to calling \code{export.dyadic}.
}
\examples{
data(BSANet)
square.data <- BSANet$square.data
el <- BSANet$el
b <- blau(square.data, node.ids = 'person', ecology.ids = 'city', graph = el)
b <- dyadic(b, dev.range = rep(1.5, 3)) # 3 is the number of dimensions
export.dyadic(b)
}
