\name{hierarchical}
\alias{hierarchical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hierarchical clustering
}
\description{
Computes a hierarchical clustering of a data matrix at a certain threshold.
}
\usage{
singlelinkage(X,h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Data matrix}
  \item{method}{Type of hierarchical clustering method, as in \code{hclust}}
  \item{h}{Threshold}
}
\details{
The threshold \code{h} may be passed through as an additional argument in the main mappeR function.
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