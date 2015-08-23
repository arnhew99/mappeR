hierarchicalhist <-
function(X,method) {
	dX <- dist(X)
	tree <- hclust(dX,method=method)
	x <- c(tree$height,mean(dX))
	zero.found <- FALSE
	
	i <- 4
	while (!(zero.found)) {
		h.obj <- hist(x,breaks=seq(from=min(x),to=max(x),length.out=i),plot=FALSE)
		zero.found <- 0 %in% h.obj$counts
		i <- i + 1
	}
	
	return(cutree(tree,h=h.obj$mids[max(which(h.obj$counts == 0))]))
}
