\name{BSANet}
\alias{BSANet}
\docType{data}
\title{
Example dataset for relational measures.
}
\description{
The dataset is a practice dataset created to illustrate the dyadic blau status functions of Blaunet. 
}
\usage{data(BSANet)}
\format{
  The format is:\cr
List of 3\cr
 $ square.data:'data.frame':	10 obs. of  7 variables: \cr
  ..$ person          : Factor w/ 10 levels "Amir","Aude",..: 6 5 8 1 7 4 10 2 9 3\cr
  ..$ city            : Factor w/ 2 levels "New York","San Francisco": 1 1 1 1 1 2 2 2 2 2\cr
  ..$ age             : int [1:10] 24 38 58 47 33 23 28 22 29 41\cr
  ..$ income          : int [1:10] 5 1 2 4 4 0 6 1 7 5\cr
  ..$ educ            : int [1:10] 7 3 7 2 4 7 0 4 5 7\cr
  ..$ grp_conservative: int [1:10] 1 0 1 0 0 1 1 1 1 1\cr
  ..$ grp_liberal     : int [1:10] 1 1 0 0 0 0 1 1 1 1\cr
 $ el         :'data.frame':	8 obs. of  2 variables:\cr
  ..$ V1: Factor w/ 6 levels "Amir","Aude",..: 4 4 5 1 3 3 2 6\cr
  ..$ V2: Factor w/ 6 levels "Aude","Bruce",..: 3 5 3 4 6 1 6 2\cr
 $ adj        :'data.frame':	10 obs. of  10 variables:\cr
  ..$ John : int [1:10] 0 1 1 0 0 0 0 0 0 0\cr
  ..$ Emma: int [1:10] 1 0 1 0 0 0 0 0 0 0\cr
  ..$ Mary : int [1:10] 1 1 0 0 0 0 0 0 0 0\cr
  ..$ Amir : int [1:10] 0 0 0 0 1 0 0 0 0 0\cr
  ..$ Mark : int [1:10] 0 0 0 1 0 0 0 0 0 0\cr
  ..$ Bryan: int [1:10] 0 0 0 0 0 0 1 1 0 0\cr
  ..$ Wendy: int [1:10] 0 0 0 0 0 1 0 1 0 0\cr
  ..$ Aude : int [1:10] 0 0 0 0 0 1 1 0 0 0\cr
  ..$ Mona : int [1:10] 0 0 0 0 0 0 0 0 0 1\cr
  ..$ Bruce: int [1:10] 0 0 0 0 0 0 0 0 1 0\cr
}
\details{
This dataset is a small dataset containing 10 individuals in two non-overlapping locations (as cities), created solely to illustrate the functions of the Blaunet package.  Affiliation variables contains membership in a liberal or conservative organization (or both).  Demographic data contains variables of age, education, and income.  Individual-level network data is also available in both adjacency matrix and edge list formats.
}
\source{
Data was created by the Blaunet team. All data is fictional and was created with a random number generator. 
}
\value{
Returns a list containing the BSANet attribute and network files.
}
\examples{
data(BSANet)
}
\keyword{datasets}
