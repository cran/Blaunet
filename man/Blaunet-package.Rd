\name{Blaunet-package}
\alias{blaunet}
\docType{package}
\title{
Calculate and Analyze Blau statuses for Measuring Social Distance
}
\description{
Blaunet package calculates and analyzes Blau statuses for quantifying social distance between individuals belonging to organizations. Blaunet works by considering individuals placed in an abstract social space, defined by various user-provided variables. Organizations recruit from an area (or ``niche'') in social space. Blaunet analyzes where organizations recruit from, the positions users occupy in this competitive environment, and substantial features of the connections between individuals.
}
\details{
\tabular{ll}{
Package: \tab Blaunet\cr
Type: \tab Package\cr
Version: \tab 2.2.1\cr
Date: \tab 2022-09-26\cr
Depends: \tab R (>= 3.0.0)\cr
Imports: \tab digest, gWidgets2, gWidgets2tcltk, plot3D, plot3Drgl, rgl,\cr
         \tab network, sna, ergm, statnet.common, haven, foreign\cr 
License: \tab GPL-3\cr
}
\bold{1. Purpose of software}: The purpose of the software is to perform Blau status analysis, which is an analytic framework that allows one to describe how organizational competition affects micro-level processes. It relies on abstractly conceptualizing individuals as inhabiting a k-dimensional euclidean space formed by k variables.

\bold{2. Defintions} 

\bold{a. Blau Space}:  A k-dimensional space, where socio-demographic characteristics such as age, years of education, or income are used to structure the dimensions of the space. These characteristics are referred to as Blau parameters. Individuals are then populated in this space based on their attributes on the specified demographic dimensions. Each person is represented by a set of k coordinates, which correspond to the person's attributes on the Blau parameters.

\bold{b. Niche}: The area of the Blau space from which organizations recruit members. The niches are calculated based on the Blau parameters of individuals already in the organization. Organizations compete with each other both by recruiting members already inhabiting their niches and by expanding their niches. The extent to which niches overlap reflects competition between organizations for members. The niche for an organization is a hypercube in k-dimensional space.


\bold{c. Blau Status}: Denotes a person's (or a relationship's) position in the organizational competitive ecology relative to the competing niches. Blau statuses that characterize a person's position in the ecology are called nodal blau statuses. Computing nodal statuses only requires membership information. Blau statuses that characterize a relationship's position in the ecology are called dyadic blau statuses. Computing dyadic statuses requires not only membership, but also sociometric network information. 


\bold{d. Ecology}: A relatively bound system where organizations compete for members. The system is bound by spatial or other considerations. For example, a school where extracurricular clubs compete for members could represent a single ecology. A second school in another town may form a second ecology, as the second schools clubs do not recruit at the first school. A town where different voluntary organizations compete for members can also represent a single ecology. By assumption, organizations can only compete within an ecology. For instance, an Elks group can compete with a church group so long as the two draw members from the same community. However, much like the boundary specification problem (Laumann, Marsden, Prensky 1983) in social networks, ecologies are difficult to precisely define.

\bold{3. Data Structure}: Several types of information are necessary or helpful to specific the Blau statuses of individuals: 
\bold{i.)} Individual ID: this variable identifies the individuals in the dataset

\bold{ii.)} Ecology ID: indicates how individuals are partitioned into ecologies (e.g. schools, cities, etc.).

\bold{iii.)} Blau Parameters: continuous demographic characteristics of individuals that the researcher finds relevant to determining similarity among individuals (e.g. age, SES, BMI). This set of variables is used to construct the niches that structure Blau space.

\bold{iv.)} Organizational Memberships: the membership of individuals in specific organizations (e.g. charitable organizations, religious groups, fraternal organizations). This set of variables is used to construct the niches that individuals occupy.

\bold{v.)} Network data: the sociometric network information of individuals. This information is used to compute dyadic blau statuses, although is not necessary for other status computations.

\bold{vi.)} Weights (optional): for data that was sampled from a population and where Blau  parameters need to be adjusted by weights.

\bold{4. Note:} We've made every effort to make sure that that the program works correctly. However, we advise caution as we are unable to test on every type of dataset. If anything looks anomalous, please let us know at the maintenance email address below. 

}
\author{
Cheng Wang*, 'chengwang@wayne.edu'

Michael Genkin*, 'mgenkin@smu.edu.sg'

George Berry, 'geb97@cornell.edu'

Liyuan Chen,

Matthew E. Brashears, 'brasheam@mailbox.sc.edu'

*Both authors contributed equally to this work and their names are randomly ordered

Maintainer: Cheng Wang <'chengwang@wayne.edu'>
}
\references{
McPherson, J. Miller. (1983). ``An Ecology of Affiliation''. American Sociological Review, 48(4), 519-532. DOI: 10.2307/2117719

McPherson, J. Miller. (2004). ``A Blau Space Primer: Prolegomenon to an Ecology of Affiliation''. Industrial and Corporate Change, 13(1), 263-280. DOI: 10.1093/icc/13.1.263

Genkin, Michael, Cheng Wang, George Berry, and Matthew E. Brashears. (2018). ``Blaunet: An R-based Graphical User Interface Package to Analyze Blau Space''. PLoS ONE, 13(10), e0204990. DOI: 10.1371/journal.pone.0204990
}
\value{No return value} 
\keyword{ package }
