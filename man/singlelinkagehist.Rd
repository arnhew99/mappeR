\name{singlelinkagehist}
\alias{singlelinkagehist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes a single linkage clustering (histogram method).
}
\description{
Computes a clustering of a data matrix, using single linkage.
}
\usage{
singlelinkagehist(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Data matrix}
}
\details{
This a special case of the \code{hierarchicalhist} function with \code{method="single"}.
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
