singlelinkagehist <-
function(X) {
  dX <- dist(X)
  x <- c(hclust(dX,method="single")$height,mean(dX))
  zero.found <- FALSE
  i <- 4
  while (!(zero.found)) {
    h.obj <- hist(x,breaks=seq(from=min(x),to=max(x),length.out=i),plot=FALSE)
    zero.found <- 0 %in% h.obj$counts
    i <- i + 1
  }
  return(cutree(hclust(dist(X),method="single"),h=h.obj$mids[max(which(h.obj$counts == 0))]))
}
