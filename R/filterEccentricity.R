# eccentricity filter

filterEccentricity <- 
function(X,p=2) {

  N <- dim(X)[1]
  d <- dim(X)[2]
  
  return(sapply(1:N, function(i,p,Z) (sum(as.vector(rdist(matrix(Z[i,],ncol=d),Z[-i,]))^p)/N)^(1/p),p=p,Z=X))

}