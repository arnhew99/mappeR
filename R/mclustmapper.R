mclustmapper <-
function(X,...) {
	
	# allow mclust to do its thing entirely automatically
  # "best" model chosen by BIC
  # additional options passed via ...
  return(Mclust(X,...)$classification)


}
