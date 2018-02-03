#' Replicates a matrix m by n times
#'
#' @param X a matrix
#' @param m number of row replications
#' @param n number of row replications
#' @keywords matlab
#' @export
#' @examples
#' repmat(c(1,2,3), 2,5)
#' 
repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  
  if(!is.matrix(X))
  {
    X<-as.matrix(X)
  }
  
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

