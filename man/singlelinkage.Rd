\name{singlelinkage}
\alias{singlelinkage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single linkage clustering
}
\description{
Computes a single linkage clustering of a data matrix at a certain threshold.
}
\usage{
singlelinkage(X,h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Data matrix}
  \item{h}{Threshold}
}
\details{
For use as a mappeR argument, currently you need to define a function in the argument, e.g. \code{function(X) singlelinkage(X,h=0.1)} as in\cr \code{mappeR(X,n.slices,overlap,filterfn, function(X) singlelinkage(X,h=0.1))}.
}
\value{A vector containing the members (rows of X) of each cluster.}
\references{}
\author{
Matthew Arnold
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line