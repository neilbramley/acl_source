#' Get intervention values
#'
#' Computes the most useful intervention according to information, probability and utility gain.
#' Requires a dataframe called li in the global workspace that encodes the likelihoods of any outcome of any intervention created with likelihood()
#' Requires a dataframe called o which incodes the outcome space (see example)
#' Requires a matrix of interventions called ints, 1 is on, 0 is free, -1 is off.
#' @param g matrix of hypothesis graphs, one line is one graph to be written by row to a matrix.
#' @param pDist is the prior distribution over these graphs.
#' @param compute_value defaults to 0=no.  If 1 a value matrix C must be provided with a value for each judgement i given each true network j
#' Additional arguments can then be passed to this objective function
#' @keywords expected value
#' @export
#' @examples
#' ints<-as.matrix(expand.grid(rep(list(0:2), 3)))
#' ints[ints==2]<--1
#' o<-expand.grid(rep(list(0:1 ), sqrt(dim(g)[2])))
#' dist<-prior('flat',g)
#' li<-likelihood(g,.1,.8)
#' out<-choose_int(g, dist, compute_value=0)


choose_int <-
function(g, pG, compute_value=1)
{
  
  #Initialise normalisation constant (for this intervention and current prior)
  Z<-matrix(0,dim(ints)[1],dim(o)[1])
  
  #Compute Posterior and Expected Entropy/Value
  #############################################
  
  
  #Initialise matrices for Expected Entropy and Value
  EE<-matrix(0,dim(ints)[1],1)  
  EEV<-matrix(0,dim(ints)[1],1)
  Ephi<-matrix(0,dim(ints)[1],1)
  
  
  #Loop over interventions
  ########################
  for (i in 1:nrow(ints))
  {
    
    #Normalisers (for this Intervention)
    Z[i,]<-t(li[li$Interventions==i,seq(3,ncol(li),1)])%*%pG
    
    #Compute posteriors: pGgOI
    pGgOI<-li[li$Interventions==i,seq(3,ncol(li),1)]*kronecker(matrix(1,1,dim(o)[1]),pG)
    
    
    #Normalise
    pGgOI<-pGgOI/kronecker(matrix(1,dim(g)[1],1),t(as.matrix(Z[i,])))
    
    
    
    #Compute Expected Entropy for this intervention
    ###############################################
    
    #Entropy for all outcomes (marginalised over graphs)
    E<-apply(pGgOI*log2(pGgOI),2,sum,na.rm=TRUE)
    
    #Expected entropy (marignalised over outcomes)
    EE[i]<-sum(E*Z[i,]) # subtract sum(pG*log2(pG)) - for expected INCREASE in entropy
    
    
    #Compute Probability Gain for this intervention
    ###############################################
    
    #Max prob (marginalised over graphs)
    phi<-apply(pGgOI, 2,max, na.rm=TRUE,warn=0)
    
    #Expected probability (marginalised over outcomes)
    Ephi[i]<-sum(phi*Z[i,]) #-max(pG)
    
    
    if (compute_value==1)
    {
      #Compute Expected Value for this intervention
      #############################################
      
      #Expected value of proposal i (marginalised over graphs)
      EV<-C%*%as.matrix(pGgOI)
      
      #Expected increase in Value of best proposal given this intervention, and the resulting outcome
      EEV[i]<-sum(apply(EV,2,max)*Z[i,],na.rm=TRUE) #-snv
    }
    
  }
  out<-list(EE,Ephi, EEV,Z)
}
