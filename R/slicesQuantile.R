slicesQuantile <- function(filter.values, n.slices, overlap) {

	# probably going to assume overlap is 50% for the time being
	
	# want quantiles split of filter.values 
	# n.slices + 1
	qs <- quantile(filter.values, seq(0,1,length=(2*n.slices)+1))
	return(cbind(qs[seq(1,2*n.slices,by=2)], c(qs[seq(4,2*n.slices+1,by=2)], max(filter.values))))


} 