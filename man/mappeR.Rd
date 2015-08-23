\name{mappeR}
\alias{mappeR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main mappeR function
}
\description{
Runs the mappeR algorithm on a data set with the given parameters. The output is an adjacency matrix that specifies the edges between nodes (suitable for use with packages like igraph).
}
\usage{
mappeR(X, n.slices, overlap, filterfn, clusterfn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Data matrix}
  \item{n.slices}{Number of overlapping subsets to slice the data into}
  \item{overlap}{Amount of overlap between clusters (0-50)}
  \item{filterfn}{Function to filter data}
  \item{clusterfn}{Function to cluster inside subsets}
}
\details{A filter function should take as argument \code{X} and return a vector of length equal to the number of observations (rows) of \code{X}.\cr A cluster function should take as argument \code{X} (internally to mappeR, this will be the data in a slice), and return a list of cluster members.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\code{mappeR} returns an object of class "mapper".\cr\cr
An object of class "mapper" is an environment containing at least the following components:
\item{adjmat}{Node adjacency matrix}
\item{filter.values}{Vector of filter values taken by each observation in X}
\item{clusters}{List of observations in each cluster}
\item{slices}{Vector of number of clusters in each slice}
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
# compare output to pairs plot of iris data
pairs(iris[,1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

iris.mappeR.out <- mappeR(iris[,1:4],6,50,filterEccentricity,singlelinkagehist)

# find the iris types in each cluster (node)
iris.names <- lapply(iris.mappeR.out$clusters, function(x) return(substr(paste(unique(iris[x,5])),1,2)))
cluster.names <- sapply(iris.names, paste, collapse="/")

# plotting is done with igraph using the adjacency matrix in the mappeR output 
plot(iris.mappeR.out,vertex.label=cluster.names,vertex.size=30,edge.width=2)

# identify a circle
x <- runif(2000,0,2*pi)
X <- cbind(cos(x),sin(x))
plot(mappeR(X,6,50,filterCol1,singlelinkagehist))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
