# Gaussian density filter from Mapper paper

filterGauss <-
function(X,epsilon=1) {
	
	dX <- as.matrix(dist(X))
	
	x <- apply(dX,1,function(y) sum(exp(-(y^2)/epsilon)))
	
	return(x)
	
}