#' Computes Renyi entropy
#'
#' This function computes the renyi entropy of a distribution with a particular alpha function
#' @param pDist the probability distribution in question
#' @param alpha the parameter that controls the function, support = [0, Inf (alpha!=0)]
#' @param log_type defaults to 'nats'
#' @keywords entropy
#' @export
#' @examples
#' renyi_entropy(prior('random',g),2)

renyi_entropy <-
function(pDist, alpha, log_type='nats')
{
  #alpha support = [0, Inf (alpha!=0)]
  dV<-pDist
  if (log_type=='nats')
  {
    Rg<-(1/(1-alpha))*log(apply(dV^alpha, 2, sum, na.rm=T))
    
  } else if (log_type=='bits')
  {
    Rg<-(1/(1-alpha))*log2(apply(dV^alpha, 2, sum, na.rm=T))
  }
  
  Rg
}
