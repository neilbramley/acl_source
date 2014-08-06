#' Get joint probability distribution over all possible unfoldings of a cyclic graph
#'
#' Returns the marginal probability of each state at time t and of each node being active at time t
#' @param graph the generating graph
#' @param starting_state the starting position or distribution for the chain
#' @param how many steps to unfold the joint NB Very slow for more than 5ish
#' @param pow strength of causal connections, defaults to .75
#' @param sar strength of background exogenous causes of variables, defaults to 0
#' @keywords joint distribution
#' @export
#' @examples
#' get_joint(matrix(c(1,1,0,0),2,2,byrow=T), c(1,0), unfold_for=7)

get_joint<-function(graph, starting_state, unfold_for=2, pow=.75, sar=0)
{
  #Set up all states of a time slice (permutations of active/inactive elements)
  all_states<-matrix(0, 2^nrow(graph), nrow(graph))
  for (i in 1:2^nrow(graph))
  {
    all_states[i,]<-num2bin(i-1,nrow(graph))
  }
  
  ###############################
  #Lay out all possible unfoldings
  ###############################
  x<-as.matrix(rep(1:2^nrow(graph)))
  step<-1
  
  while (step<unfold_for)
  {
    tmp<-x
    if (!is.matrix(x))
    {
      x<-cbind(rep(tmp, each=2^nrow(graph)), rep(1:2^nrow(graph), length(tmp)))
    }else{
      x<-cbind(tmp[rep(seq(nrow(tmp)), each = 2^nrow(graph)), ], rep(1:2^nrow(graph), nrow(tmp)))
    }
    step<-step+1
  }
  
  ##############################################
  #Start with probability of each starting state
  ##############################################
  probs<-rep(0, nrow(all_states))
  for (i in 1:nrow(all_states))
  {
    probs[i]<-prod( abs( starting_state - (all_states[i,]==0) ) )
  }
  probs<-rep(probs, each=nrow(x)/nrow(all_states))
  
  ################################
  #Set up transition probabilities
  ################################
  transition_probs<-matrix(0,nrow(all_states),nrow(all_states))
  active_causes<-rep(0,nrow(graph))
  ps<-rep(0,nrow(graph))
  
  for (i in 1:nrow(all_states))
  {
    for (j in 1:nrow(all_states))
    {
      for (k in 1:nrow(graph))
      {
        active_causes[k]<-sum(all_states[i,graph[,k]==1]==1)
      }
      ps<-(1-(1-pow)^active_causes*(1-sar))
      
      transition_probs[i,j]<-prod(abs(ps-(all_states[j,]==0)))
    }
  }
  
  #Now compute individual probability for each unfolding
  for (i in 1:nrow(x))
  {
    p_step<-rep(0, unfold_for)
    #probability of exactly this unfolding
    for (j in 2:unfold_for)
    {
      #multiply out by probability of this step given the last
      #p(x[i,step] | x[i, step-1])
      probs[i]<-probs[i]*transition_probs[x[i,j-1], x[i,j]]
    }
  }
  
  ##########################################################
  #now return marginal probs of the states, and of the nodes
  ##########################################################
  
  #sum all outcomes which end in a particular state to get the state marginals
  marginal_states<-rep(0,nrow(all_states))
  
  for (i in 1:nrow(all_states))
  {
    marginal_states[i]<-sum(probs[x[,ncol(x)]==i])
  }
  
  #Sum over outcomes where the node is active for the node marginals
  marginal_nodes<-rep(0,ncol(graph))
  for (i in 1:ncol(graph))
  {
    marginal_nodes[i]<-sum(probs[x[,ncol(x)]%in%(which(all_states[,i]==1))])
  }
  
  out<-list(marginal_states, marginal_nodes)
  out
}