\name{filterGauss}
\alias{filterGauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gaussian filter function
}
\description{
Applies the Gaussian filter function (Singh et al., 2007) to a data matrix X, up to scaling constant.
}
\usage{
filterGauss(X,epsilon=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Data matrix}
  \item{epsilon}{Scale parameter}
}
\details{
The scale parameter is essentially the bandwidth.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Vector of filter values.
}
\references{
Singh, G., Memoli, F., Carlsson, G. (2007) Topological Methods for the Analysis of High Dimensional Data Sets and 3D Object Recognition
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
