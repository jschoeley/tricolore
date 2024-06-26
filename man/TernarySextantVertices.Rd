% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tricolore.R
\name{TernarySextantVertices}
\alias{TernarySextantVertices}
\title{Vertex Coordinates of Sextants in Equilateral Triangle}
\usage{
TernarySextantVertices(center)
}
\arguments{
\item{center}{The sextant center.
A vector of ternary coordinates [p1, p2, p3].}
}
\value{
Index, vertex id and barycentric vertex coordinates for each of the
        6 sextants.
}
\description{
Given a barycentric center coordinate return the vertex coordinates of the
of the sextant regions.
}
\examples{
# NOTE: only intended for internal use and not part of the API
tricolore:::TernarySextantVertices(rep(1/3, 3))

}
\keyword{internal}
