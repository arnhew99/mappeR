mappeR <-
function(X, n.slices, overlap, filterfn, clusterfn) {
	
	# allocate obs of X to slices based on n.slices and overlap
	
	if (is.function(filterfn)) { filter.values <- filterfn(X) } else { filter.values <- filterfn }
	filter.length <- max(filter.values) - min(filter.values)
	
	# complicated code to extract the slice limits
	slices <- seq(from=min(filter.values),to=max(filter.values),length.out=(2*n.slices + 1))
	slice.centres <- slices[seq(from=2,to=2*n.slices,by=2)]
	slice.lims <- slices[-seq(from=2,to=2*n.slices,by=2)]
	slice.lims <- cbind(slice.lims[1:n.slices],slice.lims[2:(n.slices+1)])
	overlap.x <- (slice.lims[2] - slice.lims[1])*overlap/100
	slice.lims <- cbind(slice.lims[,1] - overlap.x,slice.lims[,2] + overlap.x)

	# find out which filtervalues are in which slice
	slice.allocs <- mapply(	function(x,y,z) intersect(which(z > x),which(z < y)), 
							slice.lims[,1], 
							slice.lims[,2], 
							MoreArgs=list(z=filter.values))
							
	# find the data points in each slice
	slice.data <- lapply(slice.allocs, function(x,Z) return(Z[x,]),Z=X)
	
	# find the cluster allocations for each slice
	slice.clusters <- lapply(slice.data, clusterfn)
	
	# find out which data points are in each cluster
	slice.cluster.n <- sapply(slice.clusters,max)
	cluster.allocs <- lapply(1:n.slices, function(y) lapply(1:(slice.cluster.n[y]), function(x) slice.allocs[[y]][which(slice.clusters[[y]] == x)]))
	
	# make this list into one list
	cluster.allocs.list <- unlist(cluster.allocs,recursive=FALSE)
	
	# turn this into an adjacency matrix
	
	# if two slices share elements, then set a 1 in the adjacency matrix
	out.fn <- function(x,y) ifelse(length(intersect(x,y)==0),1,0)

	# vectorize for outer
	out.fn <- Vectorize(out.fn, vectorize.args=c("x","y"))
	adjmat <- outer(cluster.allocs.list,cluster.allocs.list,out.fn)
	
  
	diag(adjmat) <- 0
	
	res <- new.env()
	res$adjmat <- adjmat
	res$filter.values <- filter.values
	res$clusters <- cluster.allocs.list
	res$slices <- slice.cluster.n
	class(res) <- "mapper"
  
	return(res)
}
