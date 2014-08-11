print.mapper <-
function(x,...) {

  cat("Adjacency matrix\n")
  print(x$adjmat)
  cat("\nClusters in each slice\n")
  print(x$slices)
  cat("\nFilter values\n")
  print(x$filter.values)
  cat("\nCluster allocations\n")
  print(x$clusters)
  
}
