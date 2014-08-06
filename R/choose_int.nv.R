#' Choose intervention with your own choice of value function
#'
#' Computes the most useful intervention according to a target function.
#' Requires a dataframe called li in the global workspace that encodes the likelihoods of any outcome of any intervention created with likelihood()
#' Requires a dataframe called o which incodes the outcome space (see example)
#' @param ints matrix of possible interventions (rows), 1 is on, 0 is free, -1 is off.
#' @param g matrix of hypothesis graphs, one line is one graph to be written by row to a matrix.
#' @param pDist is the prior distribution over these graphs.
#' @param FUN is the objective function of your choice.
#' Additional arguments can then be passed to this objective function
#' @keywords expected value
#' @export
#' @examples
#' ints<-as.matrix(expand.grid(rep(list(0:2), 3)))
#' ints[ints==2]<--1
#' o<-expand.grid(rep(list(0:1 ), sqrt(dim(g)[2])))
#' dist<-prior('flat',g)
#' li<-likelihood(g,.1,.8)
#' choose_int.nv(ints, g, dist, FUN=shannon_entropy)

choose_int.nv <-
function(ints, g, pDist, FUN, ...)
{
  
  #Initialise variables
  Z<-matrix(0,dim(ints)[1],dim(o)[1])
  EV<-matrix(0,dim(ints)[1],1)  
  
  priorV<-FUN(pDist, ...)
  
  #Loop over interventions
  ########################
  for (i in 1:nrow(ints))
  {
    #Normalisers (for this Intervention)
    Z[i,]<-t(li[li$Interventions==i,seq(3,ncol(li),1)])%*%pDist
    
    #Compute posteriors: pGgOI
    pGgOI<-li[li$Interventions==i,seq(3,ncol(li),1)]*kronecker(matrix(1,1,dim(o)[1]),pDist)
    
    #Normalise
    pGgOI<-pGgOI/kronecker(matrix(1,dim(g)[1],1),t(as.matrix(Z[i,])))
    
    #Compute Expected Value for this intervention
    ###############################################
    
    #values for all outcomes (marginalised over graphs)
    V<-FUN(pGgOI, ...) #apply(pGgOI*log2(pGgOI),2,sum,na.rm=TRUE)
    
    #Expected entropy (marignalised over outcomes)
    EV[i]<-apply(( -( matrix(V) - priorV) * Z[i,] ), 2, sum, na.rm=T)# subtract sum(pG*log2(pG)) - for expected INCREASE in entropy
  }
  
  out<-EV
  return(out)
}
