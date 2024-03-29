% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/flag_inib.r
\name{flag_inib}
\alias{flag_inib}
\title{flag_inib}
\usage{
flag_inib(N_larvae, vol_water = 1000, critical_density = 72)
}
\arguments{
\item{N_larvae}{Number of larvae.}

\item{vol_water}{Default is 1000.}

\item{critical_density}{Larval critical density to establish if hatch is inibited.}
}
\value{
Return a logical value ( TRUE/FALSE) if inibition of hatch in the cointainer occuring.
}
\description{
Calculate if in a container exist a egg hatching inibition due to larval density.  Needs volume water average for unitarian container.
}
\author{
Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@usl2.toscana.it}
}
\references{
Janice S. Edgerly - Michelle S. Willey - Todd P. Livdahl - Ecological Entomology - Vol. 18 - Issue 2 - 1993 - pp. 123-128  \url{http://www.clarku.edu/faculty/tlivdahl/publications/livdahl-edgerly87.pdf}
}
\keyword{Egg}
\keyword{Hatch}
\keyword{inibition}

