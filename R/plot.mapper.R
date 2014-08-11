plot.mapper <-
function(x,...) {
  
  # ... is extra options to the igraph plotter
  
  # by default, colour nodes according to which slice they came from
  v.colors <- rep(1:length(x$slices),x$slices)
  v.colors <- rainbow(length(x$slices),alpha=0.7)[v.colors]

  plot(graph.adjacency(x$adjmat,mode="undirected"),vertex.color=v.colors,...)
  
}
