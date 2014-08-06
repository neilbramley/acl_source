#' Get log likelihood of a particular unfolded graph
#'
#' This function unfolds a cyclic causal graph probabilitically for a set number of time steps
#' @param X The unfolding
#' @param graph the graph for which to compute the likelihood of this unfolding
#' @param pow How likely are the causal connections to cause their effects, defaults to .75
#' @param sar How likely are the components to turn on on their own, defaults to 0.
#' @keywords cyclic networks
#' @export
#' @examples
#' likelihood_unfolding(generate_unfolding(matrix(c(1,1,0,0), 2, 2, byrow=T), c(1,0)), matrix(c(1,1,0,0), 2, 2, byrow=T))


likelihood_unfolding<-function(X, graph, pow=.75, sar=0)
{
  log_lik_X<-0
  
  #loop through time slices
  for (i in 2:ncol(X))
  {
    p_node<-rep(0,nrow(X))
    
    #loop through nodes in the current time slice
    for (j in 1:nrow(X))
    {
      
      #how many active causes at previous time-step
      active_causes<-sum(X[graph[,j]==1,i-1]==1)
      
      #what's the probability of the state of this node given its parents
      p_node[j]<-(1-(1-pow)^active_causes*(1-sar))*(X[j,i]==1) + (1-(1-(1-pow)^active_causes*(1-sar)))*(X[j,i]==0)
    }
    
    log_lik_X<-log_lik_X+sum(log(p_node))
    
  }
  
  log_lik_X
}