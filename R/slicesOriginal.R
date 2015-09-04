slicesOriginal <- function(filter.values, n.slices, overlap) {

	slices <- seq(from=min(filter.values),to=max(filter.values),length.out=(2*n.slices + 1))
	slice.centres <- slices[seq(from=2,to=2*n.slices,by=2)]
	slice.lims <- slices[-seq(from=2,to=2*n.slices,by=2)]
	slice.lims <- cbind(slice.lims[1:n.slices],slice.lims[2:(n.slices+1)])
	overlap.x <- (slice.lims[2] - slice.lims[1])*overlap/100
	slice.lims <- cbind(slice.lims[,1] - overlap.x,slice.lims[,2] + overlap.x)
	
	return(slice.lims)
}
	
	