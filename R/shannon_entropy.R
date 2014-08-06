#' Computes Shannon entropy
#'
#' This function computes the shannon entropy of a probability distribution.
#' @param pDist a probability distribution as a vector or a matrix of probability distributions columns.
#' @param log_type choose nats or bits, defaults to nats.
#' @keywords entropy
#' @export
#' @examples
#' shannon_entropy(c(.5,.5), log_type='bits')

shannon_entropy <-
function(pDist, log_type='nats')
{
  
  if (class(pDist)=='numeric')
  {
    pDist<-as.matrix(pDist)
  }
  
  dV<-pDist
  
  if (log_type=='nats')
  {
    Hg<- -apply(dV*log(dV), 2, sum, na.rm=T)
  }
  
  else if (log_type=='bits')
  {
    Hg<- -apply(dV*log2(dV), 2, sum, na.rm=T)
  }
  Hg
}
