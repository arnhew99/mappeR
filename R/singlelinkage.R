singlelinkage <-
function(X,h) {
	
	clus <- hclust(dist(X),method="single")
	
	
	# obviously better heuristics are needed, in this case this seems like a good point for this example
	return(cutree(clus,h=h))


}
