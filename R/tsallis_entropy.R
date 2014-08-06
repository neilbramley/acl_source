#' Computes Tsallis entropy
#'
#' This function computes the tsallis entropy of a distribution with a particular alpha function
#' Does not work for q=1
#' @param pDist the probability distribution in question
#' @param q the parameter that controls the function, support = [-Inf, Inf (q!=0)]
#' @keywords entropy
#' @export
#' @examples
#' tsallis_entropy(prior('random',g),2)

tsallis_entropy <-
function(pDist, q)
{
  
  dV<-pDist
  
  Tg<-(1/(q-1))*(1-apply(dV^q, 2, sum))
  
  Tg
}
