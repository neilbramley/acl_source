#' Get n-step-ahead intervention values
#'
#' Computes the most useful intervention recursively according to information, probability and utility gain.
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
#' out<-choose_int.recursive(ints, dist, n_steps=2, compute_value=0)

choose_int.recursive <-
function(ints, p_dist.old, Z.old=1, li_c.old=as.matrix(1), depth=0, n_steps=1, compute_value=1)
{
  #go deeper
  depth<-depth+1
  
  #initialise expected entropy vector (for this depth)
  EE<-rep(0,nrow(ints))
  Ephi<-rep(0,nrow(ints))
  EEV<-rep(0,nrow(ints))
  
  
  #if you are not 'at depth'
  if (depth<=n_steps)
  { 
    
    #loop over interventions 
    for (i in 1:nrow(ints))
    {
      #compute Normalisation constant.  Marginal probability of each outcome Z over graphs
      tmp<-t(li[li$Interventions==i,seq(3,ncol(li),1)]) %*% p_dist.old
      Z<-c(tmp)
      Z[is.na(Z)]<-0
      Z<-Z*rep(Z.old,each=dim(o)[1])
      
      #compute new Likelihood.
      li_c<- li_c.old[,rep(1:dim(li_c.old)[2], each=dim(o)[1])] * kronecker(matrix(1, 1, dim(p_dist.old)[2]), as.matrix(li[li$Interventions==i,seq(3,ncol(li),1)]))
      
      #compute posterior probability of graphs, given each possible outcome sequence
      p_dist.un<-li_c * t(apply(p_dist.old,1,rep,each=dim(o)[1],na.rm=1))
      
      
      #normalise
      p_dist<-p_dist.un/kronecker(matrix(1,dim(g)[1],1),t(apply(p_dist.un,2,sum)))
      
      
      #loop through the deeper loop, returning the max EE
      rtn<-choose_int.recursive(ints, p_dist, Z, li_c, depth, n_steps,compute_value)
      EE[i]<-rtn[[4]]
      Ephi[i]<-rtn[[5]]
      EEV[i]<-rtn[[6]]
      #       print(depth)
    }
    
    
    
    #if you are already 'at depth'
  }else{
    
    
    #loop through interventions
    for (i in 1:nrow(ints))
    {      
      
      #return EE
      ###################################################
      E<-apply(p_dist.old*log2(p_dist.old),2,sum,na.rm=T)
      EE[i]<-sum(E*Z.old)#(Z*rep(Z.old,each=dim(o)[1])), na.rm=T)
      
      #return probability gain
      ###############################################
      
      #max prob (marginalised over graphs)
      phi<-apply(p_dist.old, 2,max, na.rm=TRUE,warn=0)
      
      #expected probability increase (marginalised over outcomes)
      Ephi[i]<-sum(phi*Z.old) #
      
      
      if (compute_value==1)
      {
        #compute Expected Value for this intervention
        #############################################
        
        #expected value of proposal i (marginalised over graphs)
        EV<-C%*%as.matrix(p_dist.old)
        
        #Expected Value of best proposal given this intervention, and the resulting outcome
        EEV[i]<-sum(apply(EV,2,max)*Z.old,na.rm=TRUE) #-snv
      }
      
    }
    
  }
  
  
  
  
  list(EE,Ephi,EEV, max(EE, na.rm=T),max(Ephi),max(EEV))
}
