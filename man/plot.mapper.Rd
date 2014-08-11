\name{plot.mapper}
\alias{plot.mapper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a mappeR object.
}
\description{
	Uses the \code{igraph} package (primarily, \code{plot.adjacency}) to plot a mappeR object. It can pass on igraph plotting parameters.
}
\usage{
\method{plot}{mapper}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class "\code{mapper}"}
  \item{\dots}{Further arguments passed to or from other methods (including igraph plotting parameters)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Plots the mappeR object using igraph. By default, the vertex labels correspond to the order of the clusters in the mappeR object. Clusters coloured the same came from the same data slice.
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
