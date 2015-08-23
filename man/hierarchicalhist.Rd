\name{hierarchicalhist}
\alias{hierarchicalhist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes a single linkage clustering (histogram method).
}
\description{
Computes a clustering of a data matrix, using single linkage.
}
\usage{
hierarchicalhist(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Data matrix}
  \item{method}{Type of hierarchical clustering method, as in \code{hclust}}
}
\details{
The \code{method} value may be passed through to this function as an additional option to the main \code{mappeR} function.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector containing the members (rows of X) of each cluster.
}
\references{
%% ~put references to the literature/web site here ~
}
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
