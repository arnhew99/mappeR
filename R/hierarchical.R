hierarchical <- function(X, method, h) {
	dX <- dist(X)
	tree <- hclust(dX,method=method)
	clus <- cutree(tree,h=h)
	return(clus)
}
