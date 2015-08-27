# Gaussian density filter from Mapper paper

filterGauss2 <-
function(X,epsilon=1) {
	
	dX <- as.matrix(dist(X))
	
	x <- rowSums(exp(-(dX^2)/epsilon))
	
	return(x)
	
}